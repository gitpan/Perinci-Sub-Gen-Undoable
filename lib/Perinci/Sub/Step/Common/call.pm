package Perinci::Sub::Step::Common::call;

use 5.010;
use strict;
use warnings;

use Perinci::Util qw(get_package_meta_accessor);

our $VERSION = '0.21'; # VERSION

# provide 'check_or_fix' functionality for several steps: call, call_undo,
# call_riap, call_riap_undo.
sub __check_or_fix_for_call_or_call_undo {
    # whichs = which step ('call', 'call_undo', 'call_riap', 'call_riap_undo')
    # whicha = which step action ('check' or 'fix')
    # cargs  = calling function's arguments
    my ($whichs, $whicha, $cargs, $step, $step_undo) = @_;

    # undo step name
    my $us;
    if ($whichs eq 'call') {
        $us = 'Common::call_undo';
    } elsif ($whichs eq 'call_undo') {
        $us = 'Common::call';
    } else {
        die "Sorry, $whichs is not supported yet";
    }

    # XXX to support call_riap and call_riap_undo, we need to provide the Riap
    # equivalent for getting metadata and for calling the function under
    # transaction. this support has not been added, maybe later when needed.

    # for 'call' and 'call_undo', f will be a fully-qualified function name,
    # e.g. Foo::Bar::func. for 'call_riap' and 'call_riap_undo', f will be a
    # Riap URL.
    my $f         = $step->[1];
    my $fargs0    = $step->[2];
    my $undo_data = $step->[3];

    # check step arguments
    if ($whicha eq 'check') {
        my $pkg = $f; $pkg =~ s/::\w+\z// or return [
            400, "Invalid function name (not fully-qualified): $f"];
        defined &{$f} or return [400, "Function does not exist: $f"];
        if (defined $fargs0) {
            ref($fargs0) eq 'HASH'
                or return [400, "Function arguments must be hash"];
        }
        my $res = get_package_meta_accessor(package=>$pkg);
        $res->[0] == 200 or return [
            500, "Can't get meta accessor for $pkg: $res->[0] - $res->[1]"];
        my $ma   = $res->[2];
        my $leaf = $f; $leaf =~ s/.+:://;
        my $meta = $ma->get_meta($pkg, $leaf)
            or return [500, "Can't get metadata for function $f"];
        my $ff  = $meta->{features} // {};
        my $ftx = $ff->{tx} && ($ff->{tx}{use} || $ff->{tx}{req});
        unless (
            ($ftx && $ff->{undo} && $ff->{idempotent}) ||
                $ff->{pure} ||
                    ($ff->{dry_run} && $cargs->{-dry_run})) {
            return [412, "Function not eligible for transaction: $f"];
        }
        if (defined $undo_data) {
            ref($undo_data) eq 'ARRAY'
                or return [400, "Undo data must be array"];
        }
        return [400, "Please specify undo data (step->[3])"]
            if $whichs eq 'call_undo' && !$undo_data;
    }

    # fargs = arguments to call our function with
    my %fargs = %{$fargs0 // {}};
    if ($whichs eq 'call') {
        if ($undo_data) {
            $fargs{-undo_action} = 'undo';
            $fargs{-undo_data}   = $undo_data;
        } else {
            $fargs{-undo_action} = 'do';
        }
    } else {
        $fargs{-undo_action} = 'redo';
        $fargs{-undo_data}   = $undo_data;
    }
    if ($whicha eq 'check') {
        $fargs{-dry_run} = 1;
        $fargs{-log_fix} = 0;
    } else {
        $fargs{-tx_manager} = $cargs->{-tx_manager};
        $fargs{-tx_call_id} = $cargs->{-tx_call_id};
    }

    no strict 'refs';
    my $res = *{$f}{CODE}->(%fargs);

    if ($whicha eq 'check') {
        if ($res->[0] == 200 || $res->[0] == 304) {
            return [200, "OK", [$us, $f, $fargs0, $res->[3]{undo_data} // []]];
        } else {
            return [500, "$f returns failure: $res->[0] - $res->[1]"];
        }
    } else {
        return $res;
    }
}

sub spec {
    state $step = {
        summary => 'Call another undoable local function',
        description => <<'_',

Syntax: `["call", $f, $args, $undo_data]`.

$f is the fully qualified function name (e.g. `Foo::Bar::func`), $args (hashref)
is the arguments hashref (defaults to {}), and $undo_data (arrayref, optional)
is undo data from function.

During `check` phase, will check the function's metadata to see if function is
indeed eligible (undoable, transactional, supports dry run, and is idempotent)
and the call $f under dry_run mode, to get function's undo data. Will return a
`call_undo` undo step: `["call_undo", $f, $args, $undo_data]`.

During `fix` phase, will call the function (passing -tx_manager, -undo_data, et
al) and return its result.

_
        check_or_fix => sub {
            __check_or_fix_for_call_or_call_undo('call', @_);
        },
    };
    $step;
}

1;
# ABSTRACT: Call another undoable local function

__END__
=pod

=head1 NAME

Perinci::Sub::Step::Common::call - Call another undoable local function

=head1 VERSION

version 0.21

=head1 DESCRIPTION


This module has L<Rinci> metadata.

=head1 FUNCTIONS


None are exported by default, but they are exportable.

=head1 AUTHOR

Steven Haryanto <stevenharyanto@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2012 by Steven Haryanto.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut


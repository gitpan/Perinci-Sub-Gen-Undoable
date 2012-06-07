package Perinci::Sub::Gen::Undoable;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Scalar::Util qw(blessed);

our $VERSION = '0.02'; # VERSION

our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(gen_undoable_func);

our %SPEC;

$SPEC{gen_undoable_func} = {
    v           => 1.1,
    summary     => 'Generate undoable '.
        '(transactional, dry-runnable, idempotent) function',
    description => <<'_',

This function is basically a helper for writing undoable (as well as
transactional, dry-runnable, and idempotent) function. This function will
generate a function with the basic structure, and you supply the
'business-logic' bits through the 'build_steps' and 'steps' arguments.

This generator helps because writing an undoable and transactional function is a
bit tricky. This helper shields function writer from having to interact with the
transaction manager, or writing the steps loop, or writing the rollback
mechanism on his/her own, and just focus on writing the actual steps.

Some notes:

- Though it generally should not die(), generated function might still die,
  there is no big eval{} loop inside. But this is usually OK since a function
  wrapper (like Perinci::Sub::Wrapper) has this loop.

For examples, see Setup::* Perl modules.

_
    args => {
        name => {
            summary => 'Fully qualified function name',
            schema  => 'str*',
            req => 1,
        },
        summary => {
            summary => 'Generated function\'s summary',
            schema  => 'str*',
        },
        description => {
            summary => 'Generated function\'s description',
            schema  => 'śtr*',
        },
        tx => {
            summary => 'Whether function is transactional',
            schema  => [hash => {default=>{}}],
            description => <<'_',

Valid values include {use=>1} (meaning function can use transaction and will
utilize it if given '-tx' special argument), {req=>1} (meaning function always
require '-tx' and will return error response if not given it). Otherwise
function will not use transaction, undo data will be passed to function via
'-undo_data' special argument and returned by function in 'undo_data' result
metadata.

_
        },
        trash_dir => {
            summary => 'Whether function needs undo trash directory',
            schema  => [bool => {default => 0}],
        },
        args => {
            summary => 'Specification for generated function\'s arguments',
            schema  => [hash => {default=>{}}],
            description => <<'_',

This is just like the metadata property 'args'.

_
        },
        build_steps => {
            summary => 'Code to build steps',
            schema  => 'code*',
            req     => 1,
            description => <<'_',

This is the code that should build the steps. Code will be given the whole
function's arguments and should return an enveloped response. If response is not
a success one, it will be used as the function's response. Otherwise, code
should return the steps (an array). By convention, each step should be an array
like this: [NAME, ...] where the first element is the step name and the rest are
step arguments.

_
        },
        hook_check_args => {
            summary => 'Code to check function\'s arguments',
            schema  => 'code*',
            description => <<'_',

This is a (temporary?) hook to allow the generated function to check its
arguments. This should later be mostly unnecessary when Perinci::Sub::Wrapper
already integrates with Data::Sah to generate argument-checking code from
schema.

Code is given arguments as a *hashref* to be able to modify them (e.g. set
defaults, etc) and should return an enveloped response. If response is not a
success one, it will be used as the function's response.

_
        },
        steps => {
            summary => 'Steps specification',
            schema  => ['hash*', {
                values_of => ['hash*' => {
                    keys => {
                        summary => {
                            summary => 'Step summary',
                            schema  => 'str*',
                        },
                        description => {
                            summary => 'Step description',
                            schema  => 'str*',
                        },
                        gen_undo => {
                            summary => 'Code to run the step',
                            schema => 'code*',
                            req => 1,
                            description => <<'_',

Code will be passed (\%args, $step) and is expected to return the undo step (an
array). %args is arguments to function, and $step is step data previously built
by build_steps. If undo step is not needed (since step is a no-op), step will
not be performed.

_
                        },
                        run => {
                            summary => 'Code to run the step',
                            schema => 'code*',
                            req => 1,
                            description => <<'_',

Code will be passed (\%args, $step, $undo), and is expected to return an
enveloped result. %args is arguments to function, $step is step data previously
built by build_steps, and $undo is undo step data previously returned by step's
gen_undo.

_
                        },
                    },
                }],
            }],
            req     => 1,
        },
    },
};
sub gen_undoable_func {
    my %gen_args = @_;

    return [400, "Please specify name"]        unless $gen_args{name};
    return [400, "Invalid name, please use qualified name, e.g. Foo::sub"]
        unless $gen_args{name} =~ /(.+)::(.+)/;
    return [400, "Please specify steps"]       unless $gen_args{steps};
    return [400, "Please specify build_steps"] unless $gen_args{build_steps};
    my $g_tx = $gen_args{tx} || {use=>1};
    return [400, "tx must be a hash"]          unless ref($g_tx) eq 'HASH';

    my $meta = {
        v           => 1.1,
        summary     => $gen_args{summary},
        description => $gen_args{description},
        args        => $gen_args{args} // {},
        features    => {
            undo       => 1,
            dry_run    => 1,
            idempotent => 1,
            tx         => $g_tx->{req} ?
                {req=>1} : $g_tx->{use} ? {use=>1} : 0,
        },
        deps        => {},
    };
    $meta->{deps}{undo_trash_dir} = 1 if $gen_args{trash_dir};

    my $code = sub {
        my %fargs = @_;
        $log->tracef("-> %s(%s)", $gen_args{name},
                     { map {$_ => (/^(-tx_manager|-undo_data)$/ ? "..." :
                                       $fargs{$_})} keys %fargs });
        my $res;

        my $tx;
        if ($gen_args{hook_check_args}) {
            $res = $gen_args{hook_check_args}->(\%fargs);
            return [400, "Error in arguments: $res->[0] - $res->[1]"]
                unless $res->[0] == 200;
        }

        my $dry_run     = $fargs{-dry_run};
        my $undo_action = $fargs{-undo_action} // "";
        return [400, "Invalid -undo_action, please use do/undo/redo only"]
            unless !length($undo_action) || $undo_action =~ /\A(un|re)?do\z/;
        $tx             = $fargs{-tx_manager} if $g_tx->{use} || $g_tx->{req};
        my $cid         = $fargs{-tx_call_id};
        return [400, "Please supply -tx_call_id on undo/redo (with tx)"]
            if !$cid && $tx && $undo_action =~ /\A(un|re)do\z/;
        if (!$cid && $tx) {
            $res = $tx->record_call(f=>$gen_args{name}, args=>\%fargs);
            return $res unless $res->[0] == 200;
            $cid = $res->[2];
        }
        return [412, "Please supply -tx_manager"] if !$dry_run &&
            $g_tx->{req} && !$tx;
        my $save_undo   = $undo_action ? 1:0;
        if ($tx && !$save_undo) {
            $log->warnf("Transaction manager passed without -undo_action, ".
                            "might be a mistake?");
        }

        # build steps

        my $steps = [];
        if ($undo_action eq 'undo') {
            if ($tx) {
                $res = $tx->get_undo_steps(call_id=>$cid);
                return [500, "Can't get undo steps from tx manager: ".
                            "$res->[0] - $res->[1]"] unless $res->[0]==200;
                $steps = $res->[2];
            } else {
                $steps = $fargs{-undo_data}
                    or return [400, "Please supply -undo_data for undo (notx)"];
            }
        } elsif ($undo_action eq 'redo') {
            if ($tx) {
                $res = $tx->get_redo_steps(call_id=>$cid);
                return [500, "Can't get redo steps from tx manager: ".
                            "$res->[0] - $res->[1]"] unless $res->[0]==200;
                $steps = $res->[2];
            } else {
                $steps = $fargs{-undo_data}
                    or return [400, "Please supply -undo_data for redo"];
            }
            return [501, "Not yet implemented"];
        } else {
            $res = $gen_args{build_steps}->(%fargs);
            return $res unless $res->[0] == 200;
            $steps = $res->[2];
            return [500, "BUG: build_steps didn't return an array: $steps"]
                unless ref($steps) eq 'ARRAY';
            if ($gen_args{trash_dir}) {
                if ($tx) {
                    $res = $tx->get_trash_dir;
                    return [500, "Can't get trash dir: $res->[0] - $res->[1]"]
                        unless $res->[0] == 200;
                    $fargs{-undo_trash_dir} //= $res->[2];
                } else {
                    # XXX hardcoded, should probably use File::Temp or something
                    $fargs{-undo_trash_dir} //= '/tmp';
                }
                # XXX this actually has a side-effect of creating trash_dir even
                # under dry_run. is this ok?
            }
        }
        $log->tracef("steps: %s", $steps);
        if ($dry_run && @$steps) {
            return [200, "Dry run"];
        }

        # perform the steps

        my $undo_steps = [];

        # whether we are performing our own rollback (not using $tx)
        my $is_rollback;

        my $step;
        my $i;
      STEP:
        for $i (1..@$steps) {
            $step = $steps->[$i-1];
            undef $res;
            {
                unless (ref($step) eq 'ARRAY') {
                    $res = [500, "Not an array ($step)"];
                    last;
                }
                my $sspec = $gen_args{steps}{$step->[0]};
                if (!$sspec) {
                    $res = [500, "Unknown step '$step->[0]'"];
                    last;
                }
                $log->tracef("%sstep #%d/%d: %s",
                             $is_rollback ? "rollback " : "",
                             $i, scalar(@$steps), $step);
                my $undo_step = $sspec->{gen_undo}->(\%fargs, $step);
                if ($undo_step) {
                    if ($tx) {
                        my $meth = $undo_action eq 'undo' ?
                            'record_redo_step' : 'record_undo_step';
                        $res = $tx->$meth(call_id=>$cid, data=>$undo_step);
                        if ($res->[0] != 200 && $res->[0] != 304) {
                            $res = [500, "Can't record undo/redo step: ".
                                        "$res->[0] - $res->[1]"];
                            last;
                        }
                    }
                    unshift @$undo_steps, $undo_step;
                    $res = $sspec->{run}->(\%fargs, $step, $undo_step);
                } else {
                    # step is a no-op
                }
            }
            if ($res && $res->[0] != 200 && $res->[0] != 304) {
                $res = [500, "Step #$i/".scalar(@$steps).
                            ": $res->[0] - $res->[1]"];
                last STEP;
            }
        } # step

        my $res0; # store failed res before rollback

        if ($res && $res->[0] != 200 && $res->[0] != 304) {
            $log->warnf("Step failed: %s, rolling back ...", $res);
            if ($tx) {
                my $rbres = $tx->rollback;
                if ($rbres->[0] != 200) {
                    return [532, "Can't rollback: $rbres->[0] - $rbres->[1] ".
                                "(after failure: $res->[0] - $res->[1])"];
                } else {
                    return [$res->[0], "$res->[1] (rolled back, tx)"];
                }
            } elsif (!$is_rollback) {
                # perform our own rollback by performing $undo_steps as $steps
                $res0       = $res;
                $steps      = $undo_steps;
                $undo_steps = [];
                $is_rollback++;
                $log->tracef("rollback steps: %s", $steps);
                goto STEP;
            } else {
                # what can we do, abandon ...
                return [500, "Rollback failed (step #$i/".scalar(@$steps)."): ".
                            "$res->[0] - $res->[1]"];
            }
        }

        return [$res0->[0], "$res0->[1] (rolled back, notx)"] if $is_rollback;

        my $meta = {};
        $meta->{undo_data} = $undo_steps if $save_undo;
        $res = [@$steps ? 200 : 304, @$steps ? "OK" : "Nothing done", undef,
                $meta];
        # doesn't always get to here, so for consistency we don't log
        #$log->tracef("<- %s() = %s", $gen_args{name}, [$res->[0], $res->[1]]);
        $res;
    };

    no strict 'refs';
    *{ $gen_args{name} } = $code;

    [200, "OK", {code=>$code, meta=>$meta}];
}

1;
# ABSTRACT: Generate undoable (transactional, dry-runnable, idempotent) function


__END__
=pod

=head1 NAME

Perinci::Sub::Gen::Undoable - Generate undoable (transactional, dry-runnable, idempotent) function

=head1 VERSION

version 0.02

=head1 SYNOPSIS

 # See an example, like in Setup::File::Symlink

=head1 DESCRIPTION

This module helps you write undoable/transactional functions (as well as
functions that support dry-run and are idempotent).

=head1 SEE ALSO

L<Rinci::function::Undo>, L<Rinci::function::Transaction>, L<Rinci>

L<Setup>.

=head1 FUNCTIONS


=head2 gen_undoable_func(%args) -> [status, msg, result, meta]

Generate undoable (transactional, dry-runnable, idempotent) function.

This function is basically a helper for writing undoable (as well as
transactional, dry-runnable, and idempotent) function. This function will
generate a function with the basic structure, and you supply the
'business-logic' bits through the 'build_steps' and 'steps' arguments.

This generator helps because writing an undoable and transactional function is a
bit tricky. This helper shields function writer from having to interact with the
transaction manager, or writing the steps loop, or writing the rollback
mechanism on his/her own, and just focus on writing the actual steps.

Some notes:

=over

=item *

Though it generally should not die(), generated function might still die,
  there is no big eval{} loop inside. But this is usually OK since a function
  wrapper (like Perinci::Sub::Wrapper) has this loop.


=back

For examples, see Setup::* Perl modules.

Arguments ('*' denotes required arguments):

=over 4

=item * B<args> => I<hash> (default: {})

Specification for generated function's arguments.

This is just like the metadata property 'args'.

=item * B<build_steps>* => I<code>

Code to build steps.

This is the code that should build the steps. Code will be given the whole
function's arguments and should return an enveloped response. If response is not
a success one, it will be used as the function's response. Otherwise, code
should return the steps (an array). By convention, each step should be an array
like this: [NAME, ...] where the first element is the step name and the rest are
step arguments.

=item * B<description>* => I<śtr>

Generated function's description.

=item * B<hook_check_args>* => I<code>

Code to check function's arguments.

This is a (temporary?) hook to allow the generated function to check its
arguments. This should later be mostly unnecessary when Perinci::Sub::Wrapper
already integrates with Data::Sah to generate argument-checking code from
schema.

Code is given arguments as a B<hashref> to be able to modify them (e.g. set
defaults, etc) and should return an enveloped response. If response is not a
success one, it will be used as the function's response.

=item * B<name>* => I<str>

Fully qualified function name.

=item * B<steps>* => I<hash>

Steps specification.

=item * B<summary>* => I<str>

Generated function's summary.

=item * B<trash_dir> => I<bool> (default: 0)

Whether function needs undo trash directory.

=item * B<tx> => I<hash> (default: {})

Whether function is transactional.

Valid values include {use=>1} (meaning function can use transaction and will
utilize it if given '-tx' special argument), {req=>1} (meaning function always
require '-tx' and will return error response if not given it). Otherwise
function will not use transaction, undo data will be passed to function via
'-undoB<data' special argument and returned by function in 'undo>data' result
metadata.

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head1 AUTHOR

Steven Haryanto <stevenharyanto@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2012 by Steven Haryanto.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut


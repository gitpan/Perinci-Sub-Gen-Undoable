package Perinci::Sub::Step::Common::call_undo;

use 5.010;
use strict;
use warnings;

our $VERSION = '0.19'; # VERSION

require Perinci::Sub::Step::Common::call;

sub spec {
    state $step = {
        summary => 'Call another undoable local function (for undo)',
        description => <<'_',

This step is the undo step for `call`. Its arguments are the same as `call`'s:
`[f, args, undo_data]`, except `undo_data` is required.

_
        check_or_fix => sub {
            Perinci::Sub::Step::Common::call::__check_or_fix_for_call_or_call_undo('call_undo', @_);
        },
    };
    $step;
}

1;
# ABSTRACT: Call another undoable local function (for undo)

__END__
=pod

=head1 NAME

Perinci::Sub::Step::Common::call_undo - Call another undoable local function (for undo)

=head1 VERSION

version 0.19

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


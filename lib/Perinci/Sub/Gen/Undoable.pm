package Perinci::Sub::Gen::Undoable;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use File::Spec;
use Perinci::Exporter;
use Perinci::Sub::Gen;
use Perinci::Sub::Wrapper qw(caller);
use Scalar::Util qw(blessed reftype);
use SHARYANTO::Log::Util qw(@log_levels);
use SHARYANTO::Package::Util qw(package_exists);

our $VERSION = '0.25'; # VERSION

our %SPEC;

$SPEC{gen_undoable_func} = {
    v           => 1.1,
    summary     => 'Generate undoable '.
        '(transactional, dry-runnable, idempotent) function',
    description => <<'_',

This function (hereby called generator, or helper) is a helper for writing
undoable (as well as transactional, dry-runnable, and idempotent) function
(hereby called generated function). Undo and transaction features are as
described in Rinci::function::Undo and Rinci::function::Transaction. Generator
will create the generated function with the basic flow and structure; you just
need to supply the necessary "business logics" bits like the `check_state` and
`fix_state` hooks.

Here is the flow of the generated function:

1. Execute `check_args` hook if supplied. Hook will be passed ($args). The task
   of the this hook is to check arguments (and possibly supply defaults). It
   should return enveloped result. If response is not success, exit with this
   response. Otherwise, continue.

2. If we are called in do mode (-undo_action => 'do' or undefined):

   2a. Execute `check_state` hook, to check state. Hook will be passed ($args).
       Hook must return enveloped result, status 304 if checking succeeds and
       fixed state (final, desired state) has been reached so nothing needs to
       be done, status 200 if checking succeeds and we are in a fixable state (a
       non-final state but fixable into the final state, also called an
       acceptable initial state), status 412 if checking succeeds but we are in
       an unfixable state (non-final state which is not fixable into the final
       state, also called an unacceptable initial state). Other status means
       checking has failed. Function should return with this result unless
       status is 200.

       If status is 200, hook must also return undo data (a list of calls that
       can be used to *undo* the 'fix_state' routine back to the initial state)
       in its result metadata, in `undo_data` key. This means, if we fail during
       fixing, we can rollback to initial state using this information.

       The undo data looks like this: C<[[f1, args1], [f2, args2], ...]>. C<fN>
       is the fully-qualified function name to call (required), C<argsN> is a
       hashref containing arguments for the function (optional, defaults to {}).

   2b. If function is called in dry run mode (-dry_run => 1), we return 200
       (along undo data from step 2a in the result metadata).

   2c. We execute `fix_state` hook to fix state to final/fixed state. Hook will
       be passed ($args, $undo_data), where $undo_data was retrieved from
       `check_state` hook. Hook must return enveloped result. Function returns
       with this result. Failure will trigger a rollback by TM.

3. If we are called in undo mode (-undo_action => 'undo', -undo_data => $calls):
   then we simply invoke TM's call() to process the undo calls.

For examples, see Setup::* Perl modules.

_
    args => {
        v => {
            summary => 'Interface version',
            schema => [int => {default=>1}],
            description => <<'_',

When Perinci::Sub::Gen::Undoable 0.22 broke backward-compatibility, v was
introduced at v=2 (previous interface at v=1).

For new interface, must be set to 2.

_
        },
        %Perinci::Sub::Gen::common_args,
        trash_dir => {
            summary => 'Whether function needs undo trash directory',
            schema  => [bool => {default => 0}],
            description => <<'_',

If set to true, generator will put `-undo_trash_dir` in function arguments.

_
        },
        args => {
            summary => 'Specification for generated function\'s arguments',
            schema  => [hash => {default=>{}}],
            description => <<'_',

This is just like the metadata property 'args'.

_
        },
        check_args => {
            summary => 'Code to check function\'s arguments',
            schema  => 'code*',
            description => <<'_',

See generator's documentation on how this hook should do and return.

_
        },
        check_state => {
            summary => 'Code to check state',
            schema  => 'code*',
            req     => 1,
            description => <<'_',

See generator's documentation on how this hook should do and return.

_
        },
        fix_state => {
            summary => 'Code to fix state',
            schema  => 'code*',
            description => <<'_',

See generator's documentation on how this hook should do and return.

_
        },
        check_or_fix_state => {
            summary => 'Code to check or fix state',
            schema  => 'code*',
            description => <<'_',

An alternative to combine check_state and fix_state in one code. Code will be
called with either ('check', $args) or ('fix', $args, $undo_data), depending on
whether check/fix is wanted.

_
        },
    } # args,
};
sub gen_undoable_func {
    my %gen_args = @_;
    my @caller = caller();

    my $v = $gen_args{v} // 1;
    if ($v == 1) {
        return[412, join(
            "",
            "$caller[0] is using old interface (v=1) of ", __PACKAGE__,
                " (v0.21 and lower). Current interface version is v=2. ",
                    "Please upgrade $caller[0] or use older version of ",
            __PACKAGE__)];
    }

    # XXX schema
    my ($uqname, $package);
    my $fqname = $gen_args{name};
    return [400, "Please specify name"] unless $fqname;
    if ($fqname =~ /(.+)::(.+)/) {
        $package = $1;
        $uqname  = $2;
    } else {
        $package = $gen_args{package} // $caller[0];
        $uqname  = $fqname;
        $fqname  = "$package\::$uqname";
    }
    return [400,"Please specify check_state&fix_state (or check_or_fix_state)"]
        unless $gen_args{check_state} && $gen_args{fix_state} ||
            $gen_args{check_or_fix_state};

    my $meta = {
        v           => 1.1,
        summary     => $gen_args{summary},
        description => $gen_args{description},
        args        => $gen_args{args} // {},
        features    => {
            undo       => 1,
            dry_run    => 1,
            idempotent => 1,
            tx         => {use=>1, req=>1},
        },
        deps        => {},
    };
    $meta->{deps}{undo_trash_dir} = 1 if $gen_args{trash_dir};

    my $code = sub {
        my %fargs = @_;
        my $res;

        if ($gen_args{check_args}) {
            $res = $gen_args{check_args}->(\%fargs);
            return [400, "Error in arguments: $res->[1]"]
                unless $res->[0] == 200;
        }

        my $dry_run     = $fargs{-dry_run};
        my $undo_action = $fargs{-undo_action} // "";
        return [400, "Invalid -undo_action, please use do/undo/redo only"]
            unless !length($undo_action) || $undo_action =~ /\A(un|re)?do\z/;
        my $tm          = $fargs{-tx_manager} or
            return [412, "This function requires transaction, ".
                        "please specify -tx_manager"];

        if ($gen_args{trash_dir}) {
            $res = $tm->get_trash_dir;
            return [500, "Can't get trash dir: $res->[0] - $res->[1]"]
                unless $res->[0] == 200;
            $fargs{-undo_trash_dir} //= $res->[2];
            # XXX this actually has a side-effect of creating trash_dir even
            # under dry_run. is this ok?
        }

        my $calls;
        my $is_rollback;

        if ($undo_action eq 'do' || !$undo_action) {

            # under do mode, we execute check_state then fix_state
            my $cof = $gen_args{check_or_fix_state};
            if ($cof) {
                $res = $cof->('check', \%fargs);
            } else {
                $res = $gen_args{check_state}->(\%fargs);
            }
            $log->tracef("Result of check_state: %s", $res)
                if $fargs{-log_call} // 1;
            return $res unless $res->[0] == 200;
            return $res if $dry_run;

            my $ud = $res->[3]{undo_data};
            $log->tracef("Fixing state ...");
            if ($cof) {
                $res = $cof->('fix', \%fargs, $ud);
            } else {
                $res = $gen_args{fix_state}->(\%fargs, $ud);
            }
            $log->tracef("Result of fix_state: %s", $res);
            if ($res->[0] == 200 || $res->[0] == 304) {
                $res->[3]{undo_data} //= $ud;
            }

            return $res; # fail, let TM do the roll back
        }

        # when in undo/redo mode, we need to perform a list of calls. note that
        # currently we don't load the function's module, wrap, get the
        # function's metadata, check function's transaction support, etc. if you
        # need all of those to be safer, use TM. our own loop is just the
        # crutch.

        $calls = $fargs{-undo_data}
            or return [400, "Please supply -undo_data for undo/redo"];
        $log->tracef("Undo calls to perform: %s", $calls);
        return $tm->call(calls => $calls, dry_run=>$dry_run);

    };

    if ($gen_args{install} // 1) {
        no strict 'refs';
        *{ $fqname } = $code;
        ${$package . "::SPEC"}{$uqname} = $meta; # XXX use Perinci::MetaAccessor
    }

    [200, "OK", {code=>$code, meta=>$meta}];
}

1;
# ABSTRACT: Generate undoable (transactional, dry-runnable, idempotent) function


__END__
=pod

=head1 NAME

Perinci::Sub::Gen::Undoable - Generate undoable (transactional, dry-runnable, idempotent) function

=head1 VERSION

version 0.25

=head1 SYNOPSIS

 # See an example, like in Setup::File::Symlink

=head1 DESCRIPTION

This module helps you write undoable/transactional functions (as well as
functions that support dry-run and are idempotent).

=head1 SEE ALSO

L<Rinci::function::Undo>, L<Rinci::function::Transaction>, L<Rinci>

L<Setup>.

=head1 DESCRIPTION


This module has L<Rinci> metadata.

=head1 FUNCTIONS


None are exported by default, but they are exportable.

=head2 gen_undoable_func(%args) -> [status, msg, result, meta]

Generate undoable (transactional, dry-runnable, idempotent) function.

This function (hereby called generator, or helper) is a helper for writing
undoable (as well as transactional, dry-runnable, and idempotent) function
(hereby called generated function). Undo and transaction features are as
described in Rinci::function::Undo and Rinci::function::Transaction. Generator
will create the generated function with the basic flow and structure; you just
need to supply the necessary "business logics" bits like the C<check_state> and
C<fix_state> hooks.

Here is the flow of the generated function:

=over

=item 1.

Execute C<check_args> hook if supplied. Hook will be passed ($args). The task
   of the this hook is to check arguments (and possibly supply defaults). It
   should return enveloped result. If response is not success, exit with this
   response. Otherwise, continue.



=item 1.

If we are called in do mode (-undo_action => 'do' or undefined):

   2a. Execute C<check_state> hook, to check state. Hook will be passed ($args).
   Hook must return enveloped result, status 304 if checking succeeds and
   fixed state (final, desired state) has been reached so nothing needs to
   be done, status 200 if checking succeeds and we are in a fixable state (a
   non-final state but fixable into the final state, also called an
   acceptable initial state), status 412 if checking succeeds but we are in
   an unfixable state (non-final state which is not fixable into the final
   state, also called an unacceptable initial state). Other status means
   checking has failed. Function should return with this result unless
   status is 200.

   If status is 200, hook must also return undo data (a list of calls that
   can be used to I<undo> the 'fix_state' routine back to the initial state)
   in its result metadata, in C<undo_data> key. This means, if we fail during
   fixing, we can rollback to initial state using this information.

   The undo data looks like this: C. C
   is the fully-qualified function name to call (required), C is a
   hashref containing arguments for the function (optional, defaults to {}).

   2b. If function is called in dry run mode (-dry_run => 1), we return 200
   (along undo data from step 2a in the result metadata).

   2c. We execute C<fix_state> hook to fix state to final/fixed state. Hook will
   be passed ($args, $undoI<data), where $undo>data was retrieved from
   C<check_state> hook. Hook must return enveloped result. Function returns
   with this result. Failure will trigger a rollback by TM.



=item 1.

If we are called in undo mode (-undoI<action => 'undo', -undo>data => $calls):
   then we simply invoke TM's call() to process the undo calls.



=back

For examples, see Setup::* Perl modules.

Arguments ('*' denotes required arguments):

=over 4

=item * B<args> => I<hash> (default: {})

Specification for generated function's arguments.

This is just like the metadata property 'args'.

=item * B<check_args> => I<code>

Code to check function's arguments.

See generator's documentation on how this hook should do and return.

=item * B<check_or_fix_state> => I<code>

Code to check or fix state.

An alternative to combine checkI<state and fix>state in one code. Code will be
called with either ('check', $args) or ('fix', $args, $undo_data), depending on
whether check/fix is wanted.

=item * B<check_state>* => I<code>

Code to check state.

See generator's documentation on how this hook should do and return.

=item * B<description> => I<str>

Generated function's description.

=item * B<fix_state> => I<code>

Code to fix state.

See generator's documentation on how this hook should do and return.

=item * B<install> => I<bool> (default: 1)

Whether to install generated function (and metadata).

By default, generated function will be installed to the specified (or caller's)
package, as well as its generated metadata into %SPEC. Set this argument to
false to skip installing.

=item * B<name>* => I<str>

Generated function's name, e.g. `myfunc`.

=item * B<package> => I<str>

Generated function's package, e.g. `My::Package`.

This is needed mostly for installing the function. You usually don't need to
supply this if you set C<install> to false.

If not specified, caller's package will be used by default.

=item * B<summary> => I<str>

Generated function's summary.

=item * B<trash_dir> => I<bool> (default: 0)

Whether function needs undo trash directory.

If set to true, generator will put C<-undo_trash_dir> in function arguments.

=item * B<v> => I<int> (default: 1)

Interface version.

When Perinci::Sub::Gen::Undoable 0.22 broke backward-compatibility, v was
introduced at v=2 (previous interface at v=1).

For new interface, must be set to 2.

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


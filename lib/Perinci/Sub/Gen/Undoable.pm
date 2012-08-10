package Perinci::Sub::Gen::Undoable;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Perinci::Exporter;
use Perinci::Sub::Gen;
use Perinci::Sub::Wrapper qw(caller);
use Scalar::Util qw(blessed reftype);
use SHARYANTO::Log::Util qw(@log_levels);
use SHARYANTO::Package::Util qw(package_exists);
use Text::sprintfn;

our $VERSION = '0.21'; # VERSION

our %SPEC;

sub get_step_spec {
    my ($name, $steps) = @_;
    $steps //= {};

    my $spec = $steps->{$name};
    if (defined($spec) && !ref($spec)) {
        $name = $spec;
        $spec = $steps->{$name};
    } # currently only allows 1 level of aliasing
    if (defined($spec) && !ref($spec)) {
        $log->warn("Multiple levels of redirection in step: $name");
        return;
    }
    unless (defined $spec) {
        # attempt to load step specification from a module first
        my $m = "Perinci::Sub::Step::$name";
        eval {
            unless (package_exists($m)) {
                my $mp = $m;
                $mp =~ s!::!/!g;
                $mp .= ".pm";
                $log->tracef("Trying to load step spec from %s ...",$m);
                my $rres = require $mp;
                $log->tracef("Loaded %s", $m) if $rres;
            }
            no strict 'refs';
            $spec = *{"$m\::spec"}{CODE}->();
        };
        $log->trace("Can't load step spec from $m: $@") if $@;
    }
    $spec;
}

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

Control flow:

1. `check_args` hook is executed, if supplied. It should return enveloped
   result. If response is not success, exit with this response.

2. If `-undo_action` argument is `do`, `build_steps` hook is executed to get
   list of steps (an arrayref). If `-undo_action` is `undo` or `redo`, steps is
   retrieved either from transaction manager (`-tx_manager` argument) or passed
   undo data (`-undo_data` argument).

3. Execute the steps. First, step's `check` hook is executed. If it returns
   undef, it means nothing needs to be done and we move on to the next step. If
   it returns an arrayref (undo step), it means we need to record the undo step
   first to transaction manager (if we are using transaction) or just collect
   the step in an array (if not using transaction), and then execute the step's
   `fix` hook. The hook should return an enveloped result. If response if not
   success, we trigger the rollback mechanism (see point 4). Otherwise we move
   on to the next step. After all the steps are executed successfully, we return
   200 response.

4. To rollback: if we are not using transaction, we get out of the loop in point
   3) and enter a loop to execute the undo steps in backward order (which is
   basically the same as point 3). If we are using transaction, we call
   transaction manager's rollback() (which will also essentially do the same
   kind of loop). If there is failure in the rollback steps, we just exit with
   the last step's response.

Additional notes:

- Generated function returns result as well as result metadata (3rd and 4th
  element of envelope array). Currently each step's `fix` hook is passed these
  two thus given the opportunity to fill/change them. Result is initially {}
  like result metadata. If result is still {} at the end of executing steps,
  `undef` is used instead.

_
    args => {
        %Perinci::Sub::Gen::common_args,
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
            schema  => ['any*' => {of=>['code*', 'array*']}],
            req     => 1,
            description => <<'_',

This is the code that should build the steps. Code will be given (\%args) and
should return an enveloped response. If response is not a success one, function
will exit with that response. Steps must be an array of steps, where each step
is like this: [NAME, ...] (an array with step name as the first element and step
argument(s) name, if any, in the rest of the elements).

Each step must be defined in the `steps` argument.

_
        },
        check_args => {
            summary => 'Code to check function\'s arguments',
            schema  => 'code*',
            description => <<'_',

This is a hook to allow the generated function to check its arguments. This
should later be mostly unnecessary when Perinci::Sub::Wrapper already integrates
with Data::Sah to generate argument-checking code from schema.

Code is given (\%args), where you can modify the args (e.g. set defaults, etc)
and it will be carried on to the other steps like 'build_steps'. Code should
return enveloped result. If response is not a success one, it will be used as
the function's response.

_
        },
        steps => {
            summary => 'Steps specification',
            description => <<'_',

A hash of step name and step specification ("stepspec"). Stepspec is a hash.
Stepspec must at least have `check` and `fix` routines (or a single combined
`check_and_fix` routine).

Instead of stepspec hash, a string can also be provided and it is assumed to be
an alias to another step name.

If a step is not found in this `steps` argument, it will be searched in
Perinci::Sub::Step::<NAME> module. A step module should contain spec() which
returns the stepspec. If the module is not found, an 'unknown step' error is
returned.

_
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
                        check => {
                            summary => "Code to check step's state",
                            schema => ['any*' => {of=>['code*','array*']}],
                            req => 0, # XXX req if check_of_fix not specified
                            description => <<'_',

Code will be passed (\%args, $step) and is expected to return an enveloped
result. If checking is success, it should return status 200 and result the undo
step (an array) if a fix of state is necessary, or undef if fix is unnecessary.
%args is arguments to function, and $step is step data previously built by
`build_steps`.

_
                        },
                        fix => {
                            summary => "Code to fix the step's state",
                            schema => 'code*',
                            req => 0, # XXX req if check_of_fix not specified
                            description => <<'_',

Code will be passed (\%args, $step, $undo, $r, $rmeta), and is expected to
return an enveloped result. %args is arguments to function, $step is step data
previously built by build_steps, and $undo is undo step data previously returned
by step's `check` hook. $r is the result hashref that will be returned by the
generated function. $rmeta is the result metadata hashref that will be returned
by the generated function.

_
                        },
                        check_or_fix => {
                            summary => "Combined alternative for check+fix",
                            schema => 'code*',
                            req => 0, # XXX req if check/fix not specified
                            description => <<'_',

This is an alternative to specifying check and fix separately, and can be more
convenient for some if there are some shared code between 'check' and 'fix'.

Code will be passed ($which, \%args, $step, $undo) where $which is either
'check' or 'fix'.

_
                        },
                        fix_log_message => {
                            summary => 'String format to use to display '.
                                'log message when performing step',
                            schema => ['any*' => {of=>['str*', 'code*']}],
                            description => <<'_',

Format string that will be passed to Text::sprintfn::sprintfn to format the log
message. Variables available to sprintfn() will be the function arguments, along
with `_idx` (the step number, starts from 1), `_step` (the step name),
`_step_arg0` .. `_step_argN` (step's arguments). For example:

    "Deleting %(_step_arg0)s ..."

If not supplied, the default log message is something like:

    "Performing step: %(_step) ..."

Alternatively, `fix_log_message` can also be a coderef. It will be passed
(\%args, $step) and should return a string (the log message). The log message
will *not* be passed to sprintfn() and instead should be the final message to
display.

_
                        },
                    },
                }],
            }],
            req     => 1,
        },
        log_level_fix_step => {
            summary => 'At what level to show log messages when '.
                'performing step',
            schema  => [str => {default=>'info', in=>\@log_levels}],
            description => <<'_',

By default, it is at `info` (can be turned on using --verbose or VERBOSE=1 in
command-line scripts). For example:

    % VERBOSE=1 u-trash --dry-run *.bak
    [info] (DRY) Trashing file1.bak ...
    [info] (DRY) Trashing file2.bak ...
    [info] (DRY) Trashing file3.bak ...

However, if you feel it is too chatty, you can lower it to 'trace', for example.

_
        },
    } # args,
};
sub gen_undoable_func {
    my %gen_args = @_;

    # XXX schema
    my ($uqname, $package);
    my $fqname = $gen_args{name};
    return [400, "Please specify name"] unless $fqname;
    my @caller = caller();
    if ($fqname =~ /(.+)::(.+)/) {
        $package = $1;
        $uqname  = $2;
    } else {
        $package = $gen_args{package} // $caller[0];
        $uqname  = $fqname;
        $fqname  = "$package\::$uqname";
    }
    return [400, "Please specify steps"]       unless $gen_args{steps};
    while (my ($sname, $stepspec) = each %{$gen_args{steps}}) {
        return [400, "Invalid step name, please only use alphanums"]
            unless $sname =~ /\A\w+(::\w+)*\z/;
        #return [400, "Please specify check+fix or check_or_fix only"]
        #    unless $stepspec->{check} && $stepspec->{fix} xor
        #        $stepspec->{check_or_fix};
    }
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
        $log->tracef("-> %s(%s)", $fqname,
                     { map {$_ => (/^(-tx_manager|-undo_data)$/ ? "..." :
                                       $fargs{$_})} keys %fargs });
        my $res;

        my $tx;
        if ($gen_args{check_args}) {
            $res = $gen_args{check_args}->(\%fargs);
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
            $res = $tx->record_call(f=>$fqname, args=>{@_});
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

        # whether we are performing our own rollback (not using $tx)
        my $is_rollback;
        $is_rollback++ if $fargs{-tx_action} &&
            $fargs{-tx_action} eq 'rollback';

        # build steps

        my $steps = [];
        if ($undo_action eq 'undo') {
            $steps = $fargs{-undo_data}
                or return [400, "Please supply -undo_data for undo"];
        } elsif ($undo_action eq 'redo') {
            $steps = $fargs{-undo_data}
                or return [400, "Please supply -undo_data for redo"];
        } else {
            my $bs = $gen_args{build_steps};

            if ((reftype($bs) // '') eq 'CODE') {
                $res = $bs->(\%fargs);
                return $res unless $res->[0] == 200;
                $steps = $res->[2];
            } else {
                $steps = [@$bs];
            }
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

        my $r     = {};
        my $rmeta = {};

        # perform the steps

        my $undo_steps = [];

        my $step;
        my $i = 0;
      STEP:
        while ($i < @$steps) {
            $i++;
            $step = $steps->[$i-1];
            undef $res;
            {
                unless (ref($step) eq 'ARRAY') {
                    $res = [500, "Not an array ($step)"];
                    last;
                }
                my $stepspec = get_step_spec($step->[0], $gen_args{steps})
                    or return [500, "Unknown step: $step->[0]"];

                my $ll = $gen_args{log_level_fix_step} // 'info';
                if ($ll && ($fargs{-log_fix} // 1)) {
                    my $lm = $stepspec->{fix_log_message} //
                        "Performing step: %(_step)s ...";
                    if ((reftype($lm) // '') eq 'CODE') {
                        $lm = $lm->(\%fargs, $step);
                    } else {
                        $lm = sprintfn($lm, {
                            %fargs,
                            _idx  => $i,
                            _step => $step->[0],
                            map {
                                "_step_arg".($_-1) =>
                                    $step->[$_],
                                } 1..@$step-1,
                        });
                    }
                    my $meth = "${ll}f";
                    $log->$meth("%s%s",
                                $is_rollback ? "(ROLLBACK) " :
                                    $dry_run ? "(DRY) " : "",
                                $lm,
                            );
                }

                my $cof = $stepspec->{check_or_fix};
                my ($cres, $undo_step);
                if ($cof) {
                    $cres = $cof->('check', \%fargs, $step);
                } else {
                    $cres = $stepspec->{check}->(\%fargs, $step);
                }
                if ($cres->[0] != 200 && $cres->[0] != 304) {
                    $res = [$cres->[0], "Can't check: $cres->[1]"];
                    last;
                }
                $undo_step = $cres->[2];

                if ($undo_step) {
                    # 'check' returns an undo step, this means we need
                    # to run the 'fix' hook
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
                    unless ($dry_run) {
                        if ($cof) {
                            $res = $cof->('fix', \%fargs, $step, $undo_step,
                                          $r, $rmeta);
                        } else {
                            $res = $stepspec->{fix}->(
                                \%fargs, $step, $undo_step, $r, $rmeta);
                        }
                    }
                } else {
                    # step is a no-op, nothing needs to be done
                }
            } # block

            if ($res && $res->[0] != 200 && $res->[0] != 304) {
                $res = [500, ($is_rollback ? "rollback ": "").
                        "step #$i/".scalar(@$steps). ": $res->[0] - $res->[1]"];
                last STEP;
            }
        } # step

        my $status;
        my $msg;

        # $res not set at all under dry-run
        if ($dry_run) {
            if (@$steps) {
                ($status, $msg) = (200, "Dry run");
            } else {
                ($status, $msg) = (304, "Nothing to do");
            }
            goto RET;
        }

        my $res0; # store failed res before rollback
        $res0 //= $res;

        if ($res && $res->[0] != 200 && $res->[0] != 304) {
            $log->warnf("Step failed: %s%s", $res,
                        $is_rollback ? "" : ", rolling back ...");

            if ($tx && !$is_rollback) {
                my $rbres = $tx->rollback;
                if ($rbres->[0] != 200) {
                    return [532, "Can't rollback: $rbres->[0] - $rbres->[1] ".
                                "(after failure: $res->[0] - $res->[1])"];
                } else {
                    return [$res->[0], "$res->[1] (rolled back, tx)"];
                }
            } elsif (!$is_rollback) {
                # perform our own rollback by performing $undo_steps as $steps
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

      RET:
        $r = undef unless keys %$r;
        $rmeta->{undo_data} = $undo_steps if $save_undo;
        unless ($status) {
            if (@$undo_steps) {
                ($status, $msg) = (200, "Done");
            } else {
                ($status, $msg) = (304, "Nothing done");
            }
        }
        $res = [$status, $msg, $r, $rmeta];

        # doesn't always get to here, so for consistency we don't log
        #$log->tracef("<- %s() = %s", $fqname, [$res->[0], $res->[1]]);
        $res;
    };

    if ($gen_args{install} // 1) {
        no strict 'refs';
        *{ $fqname } = $code;
        ${$package . "::SPEC"}{$uqname} = $meta;
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

version 0.21

=head1 SYNOPSIS

 # See an example, like in Setup::File::Symlink

=head1 DESCRIPTION

This module helps you write undoable/transactional functions (as well as
functions that support dry-run and are idempotent).

=head1 FUNCTIONS

=head2 get_step_spec($name, \%steps) => HASHREF

C<%steps> is optional. Step C<$name> will first be searched in C<%steps>'s keys.
If found and value is a hashref, that stepspec is returned. If found but value
is a string, it is assumed to be the new step name (an alias) and the new step
name is searched. If not found in C<%steps> (or C<%steps> if not provided), then
an attempt is made to load module Perinci::Sub::Step::<$name>; the module should
contain C<spec()> which if called returns the stepspec.

=head1 SEE ALSO

L<Rinci::function::Undo>, L<Rinci::function::Transaction>, L<Rinci>

L<Setup>.

=head1 DESCRIPTION


This module has L<Rinci> metadata.

=head1 FUNCTIONS


None are exported by default, but they are exportable.

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

Control flow:

=over

=item 1.

C<check_args> hook is executed, if supplied. It should return enveloped
   result. If response is not success, exit with this response.



=item 1.

If C<-undo_action> argument is C<do>, C<build_steps> hook is executed to get
   list of steps (an arrayref). If C<-undo_action> is C<undo> or C<redo>, steps is
   retrieved either from transaction manager (C<-tx_manager> argument) or passed
   undo data (C<-undo_data> argument).



=item 1.

Execute the steps. First, step's C<check> hook is executed. If it returns
   undef, it means nothing needs to be done and we move on to the next step. If
   it returns an arrayref (undo step), it means we need to record the undo step
   first to transaction manager (if we are using transaction) or just collect
   the step in an array (if not using transaction), and then execute the step's
   C<fix> hook. The hook should return an enveloped result. If response if not
   success, we trigger the rollback mechanism (see point 4). Otherwise we move
   on to the next step. After all the steps are executed successfully, we return
   200 response.



=item 1.

To rollback: if we are not using transaction, we get out of the loop in point
   3) and enter a loop to execute the undo steps in backward order (which is
   basically the same as point 3). If we are using transaction, we call
   transaction manager's rollback() (which will also essentially do the same
   kind of loop). If there is failure in the rollback steps, we just exit with
   the last step's response.



=back

Additional notes:

=over

=item *

Generated function returns result as well as result metadata (3rd and 4th
  element of envelope array). Currently each step's C<fix> hook is passed these
  two thus given the opportunity to fill/change them. Result is initially {}
  like result metadata. If result is still {} at the end of executing steps,
  C<undef> is used instead.


=back

Arguments ('*' denotes required arguments):

=over 4

=item * B<args> => I<hash> (default: {})

Specification for generated function's arguments.

This is just like the metadata property 'args'.

=item * B<build_steps>* => I<array|code>

Code to build steps.

This is the code that should build the steps. Code will be given (\%args) and
should return an enveloped response. If response is not a success one, function
will exit with that response. Steps must be an array of steps, where each step
is like this: [NAME, ...] (an array with step name as the first element and step
argument(s) name, if any, in the rest of the elements).

Each step must be defined in the C<steps> argument.

=item * B<check_args> => I<code>

Code to check function's arguments.

This is a hook to allow the generated function to check its arguments. This
should later be mostly unnecessary when Perinci::Sub::Wrapper already integrates
with Data::Sah to generate argument-checking code from schema.

Code is given (\%args), where you can modify the args (e.g. set defaults, etc)
and it will be carried on to the other steps like 'build_steps'. Code should
return enveloped result. If response is not a success one, it will be used as
the function's response.

=item * B<description> => I<str>

Generated function's description.

=item * B<install> => I<bool> (default: 1)

Whether to install generated function (and metadata).

By default, generated function will be installed to the specified (or caller's)
package, as well as its generated metadata into %SPEC. Set this argument to
false to skip installing.

=item * B<log_level_fix_step> => I<str> (default: "info")

At what level to show log messages when performing step.

By default, it is at C<info> (can be turned on using --verbose or VERBOSE=1 in
command-line scripts). For example:

    % VERBOSE=1 u-trash --dry-run *.bak
    [info] (DRY) Trashing file1.bak ...
    [info] (DRY) Trashing file2.bak ...
    [info] (DRY) Trashing file3.bak ...

However, if you feel it is too chatty, you can lower it to 'trace', for example.

=item * B<name>* => I<str>

Generated function's name, e.g. `myfunc`.

=item * B<package> => I<str>

Generated function's package, e.g. `My::Package`.

This is needed mostly for installing the function. You usually don't need to
supply this if you set C<install> to false.

If not specified, caller's package will be used by default.

=item * B<steps>* => I<hash>

Steps specification.

A hash of step name and step specification ("stepspec"). Stepspec is a hash.
Stepspec must at least have C<check> and C<fix> routines (or a single combined
C<check_and_fix> routine).

Instead of stepspec hash, a string can also be provided and it is assumed to be
an alias to another step name.

If a step is not found in this C<steps> argument, it will be searched in
Perinci::Sub::Step:: module. A step module should contain spec() which
returns the stepspec. If the module is not found, an 'unknown step' error is
returned.

=item * B<summary> => I<str>

Generated function's summary.

=item * B<trash_dir> => I<bool> (default: 0)

Whether function needs undo trash directory.

=item * B<tx> => I<hash> (default: {})

Whether function is transactional.

Valid values include {use=>1} (meaning function can use transaction and will
utilize it if given '-tx' special argument), {req=>1} (meaning function always
require '-tx' and will return error response if not given it). Otherwise
function will not use transaction, undo data will be passed to function via
'-undoI<data' special argument and returned by function in 'undo>data' result
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


use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Perinci::Sub::Gen::Undoable qw(gen_undoable_func);
use Test::More 0.96;

sub test_gen {
    my (%targs) = @_;

    subtest $targs{test_name} => sub {
        my $res;
        my %fargs = (
            name        => $targs{func_name},
            summary     => $targs{summary},
            description => $targs{description},
            steps       => $targs{steps},
            build_steps => $targs{build_steps},
        );
        if ($targs{other_args}) {
            while (my ($k, $v) = each %{$targs{other_args}}) {
                $fargs{$k} = $v;
            }
        }
        eval { $res = gen_undoable_func(%fargs) };
        my $eval_err = $@;
        diag "died during function: $eval_err" if $eval_err;

        if ($targs{dies}) {
            ok($eval_err, "dies");
        }

        if ($targs{status}) {
            is($res->[0], $targs{status}, "status = $targs{status}") or
                do { diag explain $res; return };
        }

        if ($res->[0] == 200) {
            my $func = $res->[2]{code};
            my $meta = $res->[2]{meta};
            is(ref($func), 'CODE', 'func returned');
            is(ref($meta), 'HASH', 'meta returned');
        }

        if ($targs{post_test}) {
            $targs{post_test}->($res);
        }
    };
}

1;

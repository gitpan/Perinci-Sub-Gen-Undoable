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
            v => 2,
            name               => $targs{func_name},
            summary            => $targs{summary},
            description        => $targs{description},
            check_state        => $targs{check_state},
            fix_state          => $targs{fix_state},
            check_or_fix_state => $targs{check_or_fix_state},
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
            return;
        } else {
            ok(!$eval_err, "doesn't die") or do {
                diag "dies: $eval_err";
                return;
            };
        }

        $targs{status} //= 200;
        is($res->[0], $targs{status}, "status = $targs{status}") or
            do { diag explain $res; return };

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

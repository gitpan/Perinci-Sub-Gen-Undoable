#!perl

# test meta generation and the generated meta

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib $Bin, "$Bin/t";

use Test::More 0.96;
require "testlib.pl";

test_gen(
    test_name   => 'basics',
    func_name   => 'main::f',
    check_state => sub {},
    fix_state   => sub {},
    post_test   => sub {
        my $res  = shift;
        my $meta = $res->[2]{meta};
        ok($meta->{features}{undo}, 'meta: features: undo=1');
        ok($meta->{features}{idempotent}, 'meta: features: idempotent=1');
        is_deeply($meta->{features}{tx}, {use=>1}, 'meta: features: tx');
        ok($meta->{features}{undo}, 'meta: features: undo=1');
    },
);

# XXX check tx => 0

DONE_TESTING:
done_testing();

#!perl

# test the generated function

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use Log::Any '$log';
use lib $Bin, "$Bin/t";

use Test::More 0.96;
require "testlib.pl";

ok(1);

# XXX func: check unknown steps
# XXX func: check missing -tx_manager
# XXX func: check missing -tx_call_id
# XXX func: check invalid -undo_data
# XXX func: test transactions
# XXX func: test using Test::Setup

DONE_TESTING:
done_testing();

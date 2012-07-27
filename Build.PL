
use strict;
use warnings;

use Module::Build 0.3601;


my %module_build_args = (
  "build_requires" => {
    "Module::Build" => "0.3601",
    "Test::More" => "0.96"
  },
  "configure_requires" => {
    "Module::Build" => "0.3601"
  },
  "dist_abstract" => "Generate undoable (transactional, dry-runnable, idempotent) function",
  "dist_author" => [
    "Steven Haryanto <stevenharyanto\@gmail.com>"
  ],
  "dist_name" => "Perinci-Sub-Gen-Undoable",
  "dist_version" => "0.15",
  "license" => "perl",
  "module_name" => "Perinci::Sub::Gen::Undoable",
  "recommends" => {},
  "recursive_test_files" => 1,
  "requires" => {
    "Log::Any" => 0,
    "Perinci::Sub::Gen::common" => 0,
    "Rinci" => "1.1.18",
    "SHARYANTO::Log::Util" => 0,
    "Text::sprintfn" => 0,
    "perl" => "5.010001"
  },
  "script_files" => []
);


my $build = Module::Build->new(%module_build_args);

$build->create_build_script;

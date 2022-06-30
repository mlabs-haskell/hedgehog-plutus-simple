{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutus-apps ? import sources.plutus-apps { }
, deferPluginErrors ? true
, doCoverage ? false }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus plutus-apps deferPluginErrors doCoverage;
  };
in rec {
  # What should CI build?

  inherit (project) projectCoverageReport;
  inherit (project.hedgehog-plutus-simple.components) library;

  inherit (project.hedgehog-plutus-simple.components.tests) hedgehog-plutus-simple-test;

  # This will run the tests within this build and produce the test logs to stdout
  check = plutus-apps.pkgs.runCommand "run-tests" { } ''
    ${hedgehog-plutus-simple-test}/bin/hedgehog-plutus-simple-test
    echo "" > $out
  '';

}

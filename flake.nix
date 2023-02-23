{
  description = "hedgehog-plutus-simple";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]hps \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model/";
    plutarch.url = "github:Plutonomicon/plutarch-plutus/?rev=f535a6894a25e6d46d16958273769bffa8880090";
    # plutarch has a parse error on the newest commit right now
  };

  outputs = inputs@{ self, tooling, plutus-simple-model, plutarch, ... }: tooling.lib.mkFlake
    { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;

          project.extraHackage = [
            "${plutarch}"
            "${plutus-simple-model}/psm"
            "${plutus-simple-model}/cardano-simple"
          ];
          toHaddock = [
            "plutarch"
            "plutus-ledger-api"
            "cardano-ledger-alonzo"
            "cardano-ledger-babbage"
            "cardano-ledger-core"
            "cardano-ledger-shelley"
            "cardano-crypto"
            "plutus-simple-model"
          ];
        })
        ({
          flake.config.herculesCI = {
            onPush = {
              mainChecks.outputs.mainCheck = self.packages.hps-main;
              devChecks.outputs =
                builtins.mapAttrs
                  (name: { x86_64-linux ? { }, ... }: x86_64-linux)
                  self.outputs;
            };
          };
        })
      ];
      perSystem = { self', ... }: {
        packages.hps-main =
          self'.packages."hedgehog-plutus-simple:lib:hedgehog-plutus-simple".override
            { flags.dev = false; };
      };
    };
}

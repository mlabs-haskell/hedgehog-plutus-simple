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
    plutarch.url = "github:Plutonomicon/plutarch-plutus/";
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
            "plutus-simple-model"
            "cardano-simple"
            "plutus-ledger-api"
            "cardano-ledger-alonzo"
            "cardano-ledger-babbage"
            "cardano-ledger-core"
            "cardano-ledger-shelley"
            "cardano-crypto"
          ];
        })
        ({
          flake.config.herculesCI = {
            onPush = {
              mainChecks.outputs.mainCheck = self.packages.hps-production-flags;
              devChecks.outputs =
                let
                  removeMainOnly =
                    builtins.mapAttrs
                      (name: val:
                        if name == "hps-production-flags"
                        then { }
                        else if builtins.isAttrs val then removeMainOnly val else val
                      );
                in
                builtins.mapAttrs
                  (name: { x86_64-linux ? { }, ... }: removeMainOnly x86_64-linux)
                  self.outputs
              ;
            };
          };
        })
      ];
      perSystem = { self', ... }: {
        packages.hps-production-flags =
          self'.packages."hedgehog-plutus-simple:lib:hedgehog-plutus-simple".override
            { flags.dev = false; };
      };
    };
}

{
  description = "hedgehog-plutus-simple";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]hps \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix?rev=516e38050899b624d66e8e744d22a3136024a275";

    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model?ref=ari/remove-plutus-tx";

    plutarch.url = "github:Plutonomicon/plutarch-plutus";
  };

  outputs = inputs@{ self, tooling, plutus-simple-model, plutarch, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;

          project.extraHackage = [
            "${plutus-simple-model}"
            "${plutarch}"
          ];
          toHaddock = [
            "plutarch"
            "plutus-ledger-api"
            "cardano-crypto"
            "plutus-simple-model"
          ];
        })
      ];
    };
}

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
    pioneer.url = "github:input-output-hk/plutus-pioneer-program/?ref=third-iteration";
    pioneer.flake = false;
    plutus.url = "github:input-output-hk/plutus";
    plutus.follows = "tooling";
    plutus-apps.url = "github:input-output-hk/plutus-apps/?rev=41149926c108c71831cfe8d244c83b0ee4bf5c8a";

    # plutarch has a parse error on the newest commit right now
  };

  outputs = inputs@{ self, tooling, plutus-simple-model, plutarch, pioneer, plutus-apps, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;

          project.extraHackage = [
            "${plutarch}"
            "${plutus-simple-model}/psm"
            "${plutus-simple-model}/cardano-simple"
            "${pioneer}/code/week01/"
            "${plutus-apps}/plutus-ledger-constraints/"
            "${plutus-apps}/plutus-ledger/"
            "${plutus-apps}/freer-extras/"
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
      ];
    };
}

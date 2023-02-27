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
    cardano-wallet.url = "github:input-output-hk/cardano-wallet?rev=760140e238a5fbca61d1b286d7a80ece058dc729";
    cardano-wallet.flake = false;
    cardano-addresses.url = "github:input-output-hk/cardano-addresses/?rev=b7273a5d3c21f1a003595ebf1e1f79c28cd72513";
    cardano-ledger.url = "github:input-output-hk/cardano-ledger/?rev=bf008ce028751cae9fb0b53c3bef20f07c06e333";
    cardano-ledger.flake = false;

  };

  outputs =
    inputs@{ self, tooling, plutus-simple-model, plutarch, pioneer, plutus-apps, cardano-wallet, cardano-addresses, cardano-ledger,  ... }: tooling.lib.mkFlake { inherit self; }
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
            "${plutus-apps}/plutus-contract/"
            "${plutus-apps}/plutus-chain-index-core/"
            "${plutus-apps}/playground-common/"
            "${cardano-wallet}/lib/core"
            "${cardano-wallet}/lib/text-class"
            "${cardano-wallet}/lib/strict-non-empty-containers"
            "${cardano-wallet}/lib/dbvar"
            "${cardano-wallet}/lib/test-utils"
            "${cardano-wallet}/lib/numeric"
            "${cardano-addresses}/core"
            "${cardano-ledger}/byron/ledger/impl"
            "${cardano-ledger}/cardano-ledger-core"
            "${cardano-ledger}/cardano-protocol-tpraos"
            "${cardano-ledger}/eras/alonzo/impl"
            "${cardano-ledger}/eras/byron/chain/executable-spec"
            "${cardano-ledger}/eras/byron/crypto"
            "${cardano-ledger}/eras/byron/crypto/test"
            "${cardano-ledger}/eras/byron/ledger/executable-spec"
            "${cardano-ledger}/eras/byron/ledger/impl/test"
            "${cardano-ledger}/eras/shelley/impl"
            "${cardano-ledger}/eras/shelley-ma/impl"
            "${cardano-ledger}/eras/shelley/chain-and-ledger/executable-spec"
            "${cardano-ledger}/eras/shelley/test-suite"
            "${cardano-ledger}/shelley/chain-and-ledger/shelley-spec-ledger-test"
            "${cardano-ledger}/libs/non-integral"
            "${cardano-ledger}/libs/small-steps"
            "${cardano-ledger}/libs/cardano-ledger-pretty"
            "${cardano-ledger}/semantics/small-steps-test"
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

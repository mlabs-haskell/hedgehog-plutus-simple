{ plutus, pkgs ? plutus.pkgs }: rec {
  plutus_ledger_with_docs =
    plutus.plutus.haskell.packages.plutus-ledger.components.library.override {
      doHaddock = true;
      configureFlags = [ "-f defer-plugin-errors" ];
    };
}

module TxValid (txValidTests) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Data.Default

import Hedgehog qualified

import Cardano.Ledger.Language qualified as Cardano
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus

import Plutarch (compile)
import Plutarch.Prelude
import Plutarch.Script qualified as Plutarch

import Plutus.Model qualified as Model

import Cardano.Simple.PlutusLedgerApi.V1.Scripts as Simple (
  Script (Script),
  Validator (Validator),
 )

import Hedgehog.Plutus.Gen (genesisTxId, initMockState)
import Hedgehog.Plutus.ScriptContext (
  ScriptPurpose (Spending),
  ScriptTx (ScriptTx),
  scriptTx,
  scriptTxPurpose,
  scriptTxValid,
 )

txValidTests :: Hedgehog.Group
txValidTests =
  Hedgehog.Group
    "txValid"
    [
      ( "valid script"
      , Hedgehog.property $ do
          m <- Hedgehog.forAllWith Model.ppMock initMock
          Hedgehog.assert $
            scriptTxValid
              ( ScriptTx
                  { scriptTx =
                      Model.spendScript
                        validScript
                        (Plutus.TxOutRef genesisTxId 0)
                        (Plutus.Redeemer $ Plutus.toBuiltinData ())
                        (Plutus.Datum $ Plutus.toBuiltinData ())
                  , scriptTxPurpose = Spending (Plutus.TxOutRef genesisTxId 0)
                  }
              )
              m
      )
      -- ,
      --   ( "invalid script"
      --   , Hedgehog.property $ do
      --       m <- Hedgehog.forAllWith Model.ppMock initMock
      --       Hedgehog.assert . not $ scriptTxValid _ m
      --   )
    ]

initMock :: Hedgehog.Gen Model.Mock
initMock =
  initMockState
    Map.empty
    ( Map.singleton
        validHash
        ( "script output"
        , pure
            [
              ( Plutus.TxOut
                  { Plutus.txOutAddress = Plutus.scriptHashAddress validHash
                  , Plutus.txOutValue = Model.adaValue 1
                  , Plutus.txOutDatum =
                      Plutus.OutputDatum
                        (Plutus.Datum $ Plutus.toBuiltinData ())
                  , Plutus.txOutReferenceScript = Nothing
                  }
              , Nothing
              )
            ]
        )
    )
    Model.defaultBabbageV2

validScript :: Model.Versioned Model.Validator
validScript =
  Model.Versioned Cardano.PlutusV2
    . Simple.Validator
    . Simple.Script
    . Plutarch.unScript
    . either (error . Text.unpack) id
    $ compile def (plam $ \_ _ _ -> pconstant ())

validHash :: Model.ScriptHash
validHash = Model.scriptHash validScript

module TxValid (txValidTests) where

import Data.Functor.Identity (Identity (runIdentity))

import Data.Map qualified as Map
import Data.Text qualified as Text

import Data.Default

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

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
  ScriptType (Spend),
  scriptTx,
  scriptTxPurpose,
  scriptTxValid,
 )

txValidTests :: Tasty.TestTree
txValidTests =
  Tasty.testGroup
    "txValid"
    [ Tasty.testCase
        "valid script"
        ( Tasty.assertBool "Valid script fails" $
            scriptTxValid
              (tx validScript)
              (initMock validHash)
        )
    , Tasty.testCase
        "invalid script"
        ( Tasty.assertBool "invalid script passes" $
            not $
              scriptTxValid
                (tx invalidScript)
                (initMock invalidHash)
        )
    ]

initMock :: Model.ScriptHash -> Model.Mock
initMock sh =
  runIdentity $
    initMockState
      Map.empty
      ( Map.singleton
          sh
          ( "script output"
          , pure
              [
                ( Plutus.TxOut
                    { Plutus.txOutAddress = Plutus.scriptHashAddress sh
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

tx :: forall (a :: Type). Model.Versioned Model.Validator -> ScriptTx ('Spend a)
tx v =
  ScriptTx
    { scriptTx =
        Model.spendScript
          v
          (Plutus.TxOutRef genesisTxId 0)
          (Plutus.Redeemer $ Plutus.toBuiltinData ())
          (Plutus.Datum $ Plutus.toBuiltinData ())
    , scriptTxPurpose = Spending (Plutus.TxOutRef genesisTxId 0)
    }

validScript :: Model.Versioned Model.Validator
validScript =
  Model.Versioned Cardano.PlutusV2
    . Simple.Validator
    . Simple.Script
    . Plutarch.unScript
    . either (error . Text.unpack) id
    $ compile def (plam $ \_ _ _ -> pconstant ())

invalidScript :: Model.Versioned Model.Validator
invalidScript =
  Model.Versioned Cardano.PlutusV2
    . Simple.Validator
    . Simple.Script
    . Plutarch.unScript
    . either (error . Text.unpack) id
    $ compile def (plam $ \_ _ _ -> perror)

validHash :: Model.ScriptHash
validHash = Model.scriptHash validScript

invalidHash :: Model.ScriptHash
invalidHash = Model.scriptHash invalidScript

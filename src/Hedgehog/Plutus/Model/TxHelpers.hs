module Hedgehog.Plutus.Model.TxHelpers (
  payToUser,
  payToScript,
  spendPubKeyUtxo,
  spendScriptOutput,
  mintWithRed,
) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Ledger.Scripts (scriptHash)

import PlutusLedgerApi.V1 (PubKeyHash, Value)
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Plutus
import PlutusLedgerApi.V2 (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog.Plutus.Model.Internal (
  InScript (InScript),
  Script,
  ScriptSource (InTransaction),
  Tx (txInputs, txMint, txOutputs),
  TxContext (TxContext, datums, mockchain),
  TxIn (TxIn),
  TxOut (TxOut),
  convertScript,
 )

-- Utility functions should probably be in a seperate module

{- | Given a pubkeyhash value and maybe a datum produces an (unbalanced) tx
 which sends that value to that user with that datum
-}
payToUser :: PubKeyHash -> Value -> Maybe Plutus.Datum -> Tx
payToUser pkh val md =
  mempty
    { txOutputs = Vector.singleton $ TxOut (Plutus.pubKeyHashAddress pkh) val md Nothing
    }

{- | Given a script, a value, maybe a datum, and maybe a refference script
 produces an (unbalanced) tx which sends that value to that scirpt with that
 datum and refference script
-}
payToScript :: Script -> Value -> Maybe Plutus.Datum -> Maybe Script -> Tx
payToScript s v md mref =
  mempty
    { txOutputs =
        Vector.singleton $
          TxOut
            (Plutus.scriptHashAddress $ scriptHash $ convertScript s)
            v
            md
            mref
    } -- TODO if we're using datum hashes this might need to include something to add the
    -- datum hash to the table

{- | Given an outref coresponding to some pubkey utxo
 produces an (unbalanced) tx that spends that utxo
-}
spendPubKeyUtxo :: Plutus.TxOutRef -> Tx
spendPubKeyUtxo ref = mempty {txInputs = Set.singleton $ TxIn ref Nothing}

{- | Given a txcontext an outref and a redeemer produces a
 tx that spends that ouref with that redeemer
-}
spendScriptOutput :: TxContext -> Plutus.TxOutRef -> Plutus.Redeemer -> Tx
spendScriptOutput TxContext {mockchain = mock, datums} ref red =
  mempty
    { txInputs =
        Set.singleton $
          TxIn ref $
            Just $
              InScript InTransaction (Just (red, datum))
    }
  where
    outDatum =
      Plutus.txOutDatum $
        fromMaybe (error "out ref not found") $
          Map.lookup ref (Model.mockUtxos mock)
    datum = case outDatum of
      NoOutputDatum -> error "no output datum for script"
      OutputDatum dat -> dat
      OutputDatumHash dh ->
        fromMaybe (error "datum hash not found") $
          Map.lookup dh datums

{- | Given a currencySymbol token name integer and redeemer
 creates an (unbalanced) tx which mints the apropriate value with that rededemer
-}
mintWithRed :: Plutus.CurrencySymbol -> Plutus.TokenName -> Integer -> Plutus.Redeemer -> Tx
mintWithRed cs tn amt red = mempty {txMint = Map.singleton cs (Map.singleton tn amt, red)}

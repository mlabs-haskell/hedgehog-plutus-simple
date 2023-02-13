{-# LANGUAGE TypeFamilyDependencies #-}
-- TODO can this be just for Prelude?
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Hedgehog.Plutus.TxTest (
  txTest,
  omitted,
  resolveOmitted,
) where

import Prelude hiding ((.))

import Control.Arrow (first)
import Data.Kind (Constraint, Type)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

import Control.Category (Category ((.)))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus
import PlutusTx.AssocMap qualified

import Plutus.Model qualified as Model
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..))
import Hedgehog.Plutus.Adjunction (Adjunction (..))
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
  ScriptPurpose (..),
  ScriptTx (..),
 )
import Hedgehog.Plutus.TestData (
  Bad,
  Generalised,
  Good,
  TestData,
  testDataAdjunction,
 )

newtype TxTest st a
  = TxTest
      ( Model.Mock ->
        DatumOf st ->
        Adjunction (ScriptTx st) (Either (Bad a) (Good a))
      )

{- | Given an adjunction from a 'Generalised' to a 'ScriptContext', generate
a 'TxTest.

In the 'raise' direction, the supplied adjunction may omit the following
details, which will be supplied for you:

  * 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * 'txInfoRedeemers'

  * 'txInfoData'

  * 'txInfoId'

For the final three, you can use the 'omitted' function to signal this.
-}
txTest ::
  (TestData a, Plutus.FromData r) =>
  ( Model.Mock ->
    DatumOf st ->
    Adjunction (ScriptContext r st) (Generalised a)
  ) ->
  TxTest st a
txTest f = TxTest $ \mock datum ->
  testDataAdjunction
    . f mock datum
    . scriptContext mock datum

scriptContext ::
  forall st r.
  Plutus.FromData r =>
  Model.Mock ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext
  Model.Mock {Model.mockUtxos = utxos}
  _ =
    Adjunction {lower, raise}
    where
      lower :: ScriptContext r st -> ScriptTx st
      lower
        ScriptContext
          { contextTxInfo = _
          , contextPurpose
          , contextRedeemer = _
          } =
          ScriptTx
            { scriptTxPurpose = contextPurpose
            , scriptTx =
                Model.Tx
                  (error "TODO extra")
                  $ Tx
                    -- TODO is there a better option than construction the tx by hand here?
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
                    (error "TODO")
            }
      raise :: ScriptTx st -> ScriptContext r st
      raise
        ScriptTx
          { scriptTxPurpose
          , scriptTx =
            Model.Tx
              { Model.tx'extra = _
              , Model.tx'plutus =
                Tx
                  { txInputs
                  , txReferenceInputs
                  , txOutputs
                  , txFee
                  , txMint
                  , txValidRange
                  , txSignatures
                  , txRedeemers
                  , txData
                  -- ,txId
                  }
              }
          } =
          let
            convertIn :: TxIn -> Plutus.TxInInfo
            convertIn TxIn {txInRef} = Plutus.TxInInfo txInRef out
              where
                out =
                  fromMaybe (error "lookup failure") $
                    Map.lookup txInRef utxos

            adaToValue :: Model.Ada -> Plutus.Value
            adaToValue = error "TODO"
            redeemerPtr :: Plutus.RedeemerPtr
            redeemerPtr = error "TODO"
            -- TODO how can I get this?
            -- is this all wrong and I can get the redeemer from Extra?
            redeemer :: Plutus.Redeemer
            redeemer =
              fromMaybe (error "redeemer ptr not found") $
                Map.lookup redeemerPtr txRedeemers
            redeemerData :: Plutus.BuiltinData
            redeemerData = case redeemer of
              Plutus.Redeemer d -> d
           in
            ScriptContext
              { contextRedeemer =
                  fromMaybe (error "failed to parse redeemer") $
                    Plutus.fromBuiltinData redeemerData
              , contextPurpose = scriptTxPurpose
              , contextTxInfo =
                  Plutus.TxInfo
                    { Plutus.txInfoInputs = convertIn <$> Set.toList txInputs
                    , Plutus.txInfoReferenceInputs = convertIn <$> Set.toList txReferenceInputs
                    , Plutus.txInfoOutputs = txOutputs
                    , Plutus.txInfoFee = adaToValue txFee
                    , Plutus.txInfoMint = txMint
                    , Plutus.txInfoDCert = error "TODO"
                    , Plutus.txInfoWdrl = error "TODO"
                    , Plutus.txInfoValidRange = error "TODO" $ txValidRange
                    , Plutus.txInfoSignatories = Map.keys txSignatures
                    , Plutus.txInfoRedeemers = PlutusTx.fromList $ map (first (error "TODO")) $ Map.toList $ txRedeemers
                    , Plutus.txInfoData = PlutusTx.fromList $ Map.toList txData
                    , Plutus.txInfoId = error "TODO"
                    }
              }

type Omittable :: Type -> Constraint
class Omittable a
instance Omittable (PlutusTx.AssocMap.Map Plutus.ScriptPurpose Plutus.Redeemer)
instance Omittable (PlutusTx.AssocMap.Map Plutus.DatumHash Plutus.Datum)
instance Omittable Plutus.TxId

omitted :: (Omittable a) => a
omitted = undefined

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted _mock _d txinfo =
  txinfo
    { Plutus.txInfoInputs = []
    , Plutus.txInfoReferenceInputs = []
    , Plutus.txInfoOutputs = []
    , Plutus.txInfoData = error "TODO"
    }

-- txTestRight ::
--   forall (ingrs :: Type).
--   TxTest ingrs ->
--   Model.Tx ->
--   Either (Bad ingrs) ingrs
-- txTestRight (TxTest Adjunction {raise}) = raise

-- txTestTestBad ::
--   forall (m :: Type -> Type) (ingrs :: Type).
--   ( Hedgehog.MonadTest m
--   , Eq ingrs
--   , Show ingrs
--   , Eq (Bad ingrs)
--   , Show (Bad ingrs)
--   ) =>
--   TxTest ingrs ->
--   Bad ingrs ->
--   m ()
-- txTestTestBad tt bad = txTestRight tt (txTestBad tt bad) === Left bad

-- txTestTestGood ::
--   forall (m :: Type -> Type) (ingrs :: Type).
--   ( Hedgehog.MonadTest m
--   , Eq ingrs
--   , Show ingrs
--   , Eq (Bad ingrs)
--   , Show (Bad ingrs)
--   ) =>
--   TxTest ingrs ->
--   ingrs ->
--   m ()
-- txTestTestGood tt good = txTestRight tt (txTestGood tt good) === Right good

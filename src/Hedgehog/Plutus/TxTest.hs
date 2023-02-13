{-# LANGUAGE TypeFamilyDependencies #-}
-- TODO can this be just for Prelude?
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Hedgehog.Plutus.TxTest (
  txTest,
  omitted,
  resolveOmitted,
) where

import Prelude hiding ((.))

import Data.Kind (Constraint, Type)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Control.Category (Category ((.)))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.AssocMap qualified

import Plutus.Model qualified as Model

import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..))
import Hedgehog.Plutus.Adjunction (Adjunction (..))
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
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
  (TestData a) =>
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
  Model.Mock ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext _ _ =
  Adjunction
    { lower =
        \( ScriptContext
            { contextTxInfo = _
            , contextPurpose
            , contextRedeemer = _
            }
          ) ->
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
    , raise =
        ( \( ScriptTx
              { scriptTxPurpose
              , scriptTx =
                Model.Tx
                  { Model.tx'plutus =
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
              }
            ) ->
              let
                convertIn :: TxIn -> Plutus.TxInInfo
                convertIn = error "TODO"
                adaToValue :: Model.Ada -> Plutus.Value
                adaToValue = error "TODO"
               in
                ScriptContext
                  { contextRedeemer = error "TODO" $ txRedeemers
                  , -- TODO get the script from the script purpouse
                    -- lookup the reddeemer
                    -- use fromData
                    contextPurpose = scriptTxPurpose
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
                        , Plutus.txInfoRedeemers = error "TODO" $ txRedeemers
                        , Plutus.txInfoData = error "TODO" $ txData
                        , Plutus.txInfoId = error "TODO"
                        }
                  }
        )
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

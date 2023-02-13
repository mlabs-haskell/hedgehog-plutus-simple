{-# LANGUAGE TypeFamilyDependencies #-}
-- TODO can this be just for Prelude?
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Hedgehog.Plutus.TxTest (
  TxTest,
  txTest,
  omitted,
  resolveOmitted,
  txTestBadAdjunction,
  txTestGoodAdjunction,
  txTestBad,
  txTestGood,
) where

import Prelude hiding ((.))

import Control.Arrow (first)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

import Control.Category (Category ((.)))

import Prelude hiding ((.))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus

import Plutus.Model qualified as Model
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..))
import Hedgehog.Plutus.Adjunction (Adjunction (..), adjunctionTest)
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
  ScriptTx (..),
  ScriptTx (..),
  scriptTxValid,
 )
import Hedgehog.Plutus.TestData (
  Bad,
  --Generalised,
  Good,
  TestData,
  testDataAdjunction,
 )
import qualified Hedgehog

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

  * The 'txInfoInput' being spent, if this is a spend script

  * 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * The 'txInfoRedeemer' for the current script

  * 'txInfoData'

  * 'txInfoId'

Just pass empty lists/maps. For 'txInfoId', you can use the 'omitted' function.
-}
txTest ::
  (TestData a, Plutus.FromData r) =>
  ( Model.Mock ->
    DatumOf st ->
    Adjunction (ScriptContext r st) a
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

omitted :: Plutus.TxId
omitted = error "You shouldn't read this"

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted _mock _d txinfo =
  txinfo
    { Plutus.txInfoInputs = []
    , Plutus.txInfoReferenceInputs = []
    , Plutus.txInfoOutputs = []
    , Plutus.txInfoData = error "TODO"
    }

txTestBadAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  ) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBadAdjunction (TxTest f) mock datum = adjunctionTest (f mock datum) . Left

txTestGoodAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  ) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Good a ->
  m ()
txTestGoodAdjunction (TxTest f) mock datum =
  adjunctionTest (f mock datum) . Right

txTestBad ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBad (TxTest f) mock datum bad =
  Hedgehog.assert $ not (scriptTxValid ((f mock datum).lower (Left bad)) mock)

txTestGood ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Good a ->
  m ()
txTestGood (TxTest f) mock datum good =
  Hedgehog.assert $ scriptTxValid ((f mock datum).lower (Right good)) mock

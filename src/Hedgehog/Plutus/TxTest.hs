{-# LANGUAGE TypeFamilyDependencies #-}

{- HLINT ignore "Redundant bracket" -}
module Hedgehog.Plutus.TxTest (
  TxTest (TxTest),
  txTest,
  omitted,
  txTestBadAdjunction,
  txTestGoodAdjunction,
  txTestBad,
  txTestGood,
) where

import PlutusLedgerApi.V2 qualified as Plutus

import Plutus.Model qualified as Model

import Hedgehog qualified

import Hedgehog.Plutus.Adjunction (
  Adjunction (lower),
  adjunctionTest,
 )
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext,
  ScriptTx,
  scriptTxValid,
 )
import Hedgehog.Plutus.TestData (
  Bad,
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

  * The 'txInfoOutput' being spent, if this is a spend script

  * 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * 'txInfoRedeemers'

  * 'txInfoData'

  * 'txInfoId'

Just pass empty lists/maps. For 'txInfoId', you can use the 'omitted' function.
-}
txTest ::
  (TestData a) =>
  ( Model.Mock ->
    DatumOf st ->
    Adjunction (ScriptContext r st) a
  ) ->
  TxTest st a
-- txTest f = TxTest $ \mock datum ->
--   testDataAdjunction
--     . f mock datum
--     . scriptContext mock datum
txTest f = TxTest $ \mock datum -> _

scriptContext ::
  Model.Mock ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext = _

omitted :: Plutus.TxId
omitted = error "You shouldn't read this"

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted = _

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

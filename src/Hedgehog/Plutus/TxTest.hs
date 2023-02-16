{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest where

import Data.Kind (Constraint, Type)

import Prelude hiding ((.))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.AssocMap qualified

import Control.Category (Category ((.)))

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.ScriptContext
import Hedgehog.Plutus.TestData

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
txTest f = TxTest $ \mock datum ->
  testDataAdjunction
    . f mock datum
    . scriptContext mock datum

scriptContext ::
  Model.Mock ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext = _

omitted :: Plutus.TxId
omitted = undefined

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted = _

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

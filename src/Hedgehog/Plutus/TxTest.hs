{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest where

-- import Data.Kind (Type)

-- import Hedgehog ((===))
-- import Hedgehog qualified

import Prelude hiding ((.))

import Control.Category (Category ((.)))

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.ScriptContext
import Hedgehog.Plutus.TestData

newtype TxTest st a
  = TxTest
      ( Model.Mock ->
        Adjunction (ScriptTx st) (Either (Bad a) (Good a))
      )

txTest ::
  (TestData a) =>
  (Model.Mock -> Adjunction (ScriptContext d st) (Generalised a)) ->
  TxTest st a
txTest f = TxTest $ \mock -> testDataAdjunction . f mock . scriptContext mock

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

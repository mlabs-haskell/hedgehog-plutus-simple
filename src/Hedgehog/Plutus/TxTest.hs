{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest where

-- import Data.Kind (Type)

-- import Hedgehog ((===))
-- import Hedgehog qualified

import Control.Category (Category ((.)))
import Prelude hiding ((.))

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.ScriptContext
import Hedgehog.Plutus.TestData

data family Bad ingrs

newtype TxTest st a
  = TxTest
      ( Model.Mock ->
        Adjunction (ScriptTx st) (Either (Generalised a) (Good a))
      )

txTest ::
  ( Model.Mock ->
    Adjunction (ScriptContext d st) (Either (Generalised a) (Good a))
  ) ->
  TxTest st a
txTest a = TxTest $ \mock -> a mock . scriptContext mock

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

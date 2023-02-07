{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest where

-- import Data.Kind (Type)

-- import Hedgehog ((===))
-- import Hedgehog qualified

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.TestData

data family Bad ingrs

newtype TxTest a
  = TxTest (Adjunction Model.Tx (Either (Generalised a) (Good a)))

-- txTest ::
--   (Bad ingrs -> Model.Tx) ->
--   (ingrs -> Model.Tx) ->
--   (Model.Tx -> Either (Bad ingrs) ingrs) ->
--   TxTest ingrs
-- txTest b g r =
--   TxTest $
--     Adjunction
--       { lower = either b g
--       , raise = r
--       }

-- txTestBad ::
--   forall (ingrs :: Type).
--   TxTest ingrs ->
--   Bad ingrs ->
--   Model.Tx
-- txTestBad (TxTest Adjunction {lower}) = lower . Left

-- txTestGood ::
--   forall (ingrs :: Type).
--   TxTest ingrs ->
--   ingrs ->
--   Model.Tx
-- txTestGood (TxTest Adjunction {lower}) = lower . Right

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

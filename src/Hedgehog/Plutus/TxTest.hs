module Hedgehog.Plutus.TxTest where

import Data.Kind (Type)

import Hedgehog ((===))
import Hedgehog qualified

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Tx

newtype TxTest bad good = TxTest (Adjunction (Either bad good) (Tx 'Unbalanced))

txTest ::
  (bad -> Tx 'Unbalanced) ->
  (good -> Tx 'Unbalanced) ->
  (Tx 'Unbalanced -> Either bad good) ->
  TxTest bad good
txTest b g r =
  TxTest $
    Adjunction
      { left = either b g
      , right = r
      }

txTestBad ::
  forall (bad :: Type) (good :: Type).
  TxTest bad good ->
  bad ->
  Tx 'Unbalanced
txTestBad (TxTest Adjunction {left}) = left . Left

txTestGood ::
  forall (bad :: Type) (good :: Type).
  TxTest bad good ->
  good ->
  Tx 'Unbalanced
txTestGood (TxTest Adjunction {left}) = left . Right

txTestRight ::
  forall (bad :: Type) (good :: Type).
  TxTest bad good ->
  Tx 'Unbalanced ->
  Either bad good
txTestRight (TxTest Adjunction {right}) = right

txTestTestBad ::
  forall (m :: Type -> Type) (bad :: Type) (good :: Type).
  (Hedgehog.MonadTest m, Eq bad, Show bad, Eq good, Show good) =>
  TxTest bad good ->
  bad ->
  m ()
txTestTestBad tt bad = txTestRight tt (txTestBad tt bad) === Left bad

txTestTestGood ::
  forall (m :: Type -> Type) (bad :: Type) (good :: Type).
  (Hedgehog.MonadTest m, Eq bad, Show bad, Eq good, Show good) =>
  TxTest bad good ->
  good ->
  m ()
txTestTestGood tt good = txTestRight tt (txTestGood tt good) === Right good

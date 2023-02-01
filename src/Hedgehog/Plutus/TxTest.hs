{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest (
  txTest,
  ScriptTx (..),
  Bad,
  TxTest (..),
  txForScriptTx,
  txTestBad,
  txTestGood,
  txTestRight,
  txTestTestBad,
  txTestTestGood,
) where

import Data.Kind (Type)

import Hedgehog ((===))
import Hedgehog qualified

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction, left, right))
import Hedgehog.Plutus.Tx (
  Balanced (Unbalanced),
  ScriptPurpose,
  Tx,
  TxContext,
  scriptPurposeTx,
 )

data ScriptTx = ScriptTx
  { scriptTxPurpose :: ScriptPurpose
  , scriptTx :: Tx 'Unbalanced
  -- ^ The remainder of the transaction, not including the purposes's mint/spend
  }

data family Bad ingrs

txForScriptTx :: TxContext -> ScriptTx -> Tx 'Unbalanced
txForScriptTx _ctx (ScriptTx sp tx) = tx <> scriptPurposeTx sp

newtype TxTest ingrs = TxTest (Adjunction (Either (Bad ingrs) ingrs) ScriptTx)

txTest ::
  (Bad ingrs -> ScriptTx) ->
  (ingrs -> ScriptTx) ->
  (ScriptTx -> Either (Bad ingrs) ingrs) ->
  TxTest ingrs
txTest b g r =
  TxTest $
    Adjunction
      { left = either b g
      , right = r
      }

txTestBad ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  Bad ingrs ->
  ScriptTx
txTestBad (TxTest Adjunction {left}) = left . Left

txTestGood ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  ingrs ->
  ScriptTx
txTestGood (TxTest Adjunction {left}) = left . Right

txTestRight ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  ScriptTx ->
  Either (Bad ingrs) ingrs
txTestRight (TxTest Adjunction {right}) = right

txTestTestBad ::
  forall (m :: Type -> Type) (ingrs :: Type).
  ( Hedgehog.MonadTest m
  , Eq ingrs
  , Show ingrs
  , Eq (Bad ingrs)
  , Show (Bad ingrs)
  ) =>
  TxTest ingrs ->
  Bad ingrs ->
  m ()
txTestTestBad tt bad = txTestRight tt (txTestBad tt bad) === Left bad

txTestTestGood ::
  forall (m :: Type -> Type) (ingrs :: Type).
  ( Hedgehog.MonadTest m
  , Eq ingrs
  , Show ingrs
  , Eq (Bad ingrs)
  , Show (Bad ingrs)
  ) =>
  TxTest ingrs ->
  ingrs ->
  m ()
txTestTestGood tt good = txTestRight tt (txTestGood tt good) === Right good

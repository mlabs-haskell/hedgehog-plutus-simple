{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest where

import Data.Kind (Type)

import Hedgehog ((===))
import Hedgehog qualified

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Tx

data ScriptTx = ScriptTx
  { scriptTxPurpose :: ScriptPurpose
  , scriptTx :: Tx 'Unbalanced
  -- ^ The remainder of the transaction, not including the purposes's mint/spend
  }

data family Bad ingrs

txForScriptTx :: TxContext -> ScriptTx -> Tx 'Unbalanced
txForScriptTx ctx (ScriptTx sp tx) = tx <> scriptPurposeTx ctx sp

newtype TxTest ingrs = TxTest (Adjunction ScriptTx (Either (Bad ingrs) ingrs))

txTest ::
  (Bad ingrs -> ScriptTx) ->
  (ingrs -> ScriptTx) ->
  (ScriptTx -> Either (Bad ingrs) ingrs) ->
  TxTest ingrs
txTest b g r =
  TxTest $
    Adjunction
      { lower = either b g
      , raise = r
      }

txTestBad ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  Bad ingrs ->
  ScriptTx
txTestBad (TxTest Adjunction {lower}) = lower . Left

txTestGood ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  ingrs ->
  ScriptTx
txTestGood (TxTest Adjunction {lower}) = lower . Right

txTestRight ::
  forall (ingrs :: Type).
  TxTest ingrs ->
  ScriptTx ->
  Either (Bad ingrs) ingrs
txTestRight (TxTest Adjunction {raise}) = raise

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

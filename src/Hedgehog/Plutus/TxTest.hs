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

txForScriptTx :: TxContext -> ScriptTx -> Tx 'Unbalanced
txForScriptTx ctx (ScriptTx sp tx) = tx <> scriptPurposeTx ctx sp

newtype TxTest bad good = TxTest (Adjunction (Either bad good) ScriptTx)

txTest ::
  (bad -> ScriptTx) ->
  (good -> ScriptTx) ->
  (ScriptTx -> Either bad good) ->
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
  ScriptTx
txTestBad (TxTest Adjunction {left}) = left . Left

txTestGood ::
  forall (bad :: Type) (good :: Type).
  TxTest bad good ->
  good ->
  ScriptTx
txTestGood (TxTest Adjunction {left}) = left . Right

txTestRight ::
  forall (bad :: Type) (good :: Type).
  TxTest bad good ->
  ScriptTx ->
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

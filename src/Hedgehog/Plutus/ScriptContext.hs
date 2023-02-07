{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.ScriptContext where

import Data.Kind (Type)

import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog.Plutus.Adjunction

data ScriptType = Spend Type | Mint | Reward | Certify

type DatumOf :: ScriptType -> Type
type family DatumOf st where
  DatumOf ('Spend d) = d
  DatumOf _ = ()

type ScriptPurpose :: ScriptType -> Type
data ScriptPurpose st where
  Spending :: Plutus.TxOutRef -> ScriptPurpose ('Spend d)
  Minting :: Plutus.CurrencySymbol -> ScriptPurpose 'Mint
  Rewarding :: Plutus.StakingCredential -> ScriptPurpose 'Reward
  Certifying :: Plutus.DCert -> ScriptPurpose 'Certify

type ScriptContext :: Type -> ScriptType -> Type
data ScriptContext redeemer st = ScriptContext
  { contextRedeemer :: redeemer
  , contextDatum :: DatumOf st
  , contextPurpose :: ScriptPurpose st
  , contextTxInfo :: Plutus.TxInfo
  }

scriptContext :: Adjunction (ScriptContext d st) _
scriptContext = _

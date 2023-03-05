{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (ScriptContext, contextRedeemer, contextPurpose, contextTxInfo),
  ScriptPurpose (Spending, Minting, Rewarding, Certifying),
  ScriptTx (ScriptTx, scriptTx, scriptTxPurpose),
  ScriptType (Spend, Mint, Reward, Certify),
  plutusScriptContext,
  scriptTxValid,
) where

import Data.Kind (Type)

import PlutusLedgerApi.V2 qualified as Plutus

import Plutus.Model qualified as Model

data ScriptTx st = ScriptTx
  { scriptTx :: Model.Tx
  , scriptTxPurpose :: ScriptPurpose st
  }

deriving stock instance Show (ScriptPurpose st) => Show (ScriptTx st)

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

deriving stock instance Show (ScriptPurpose st)

type ScriptContext :: Type -> ScriptType -> Type
data ScriptContext redeemer st = ScriptContext
  { contextRedeemer :: !redeemer
  , contextPurpose :: !(ScriptPurpose st)
  , contextTxInfo :: !Plutus.TxInfo
  }

scriptTxValid :: ScriptTx st -> Model.Mock -> Bool
scriptTxValid = _

plutusScriptContext :: ScriptContext d st -> Plutus.ScriptContext
plutusScriptContext
  ScriptContext
    { contextTxInfo = txInfo
    , contextPurpose = sp
    } = Plutus.ScriptContext txInfo $
    case sp of
      Spending ref -> Plutus.Spending ref
      Minting cs -> Plutus.Minting cs
      Rewarding sc -> Plutus.Rewarding sc
      Certifying cert -> Plutus.Certifying cert

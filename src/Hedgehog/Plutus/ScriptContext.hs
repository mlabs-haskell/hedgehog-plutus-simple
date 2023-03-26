{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ChainState (ChainState, csMock, csScripts, csMps),
  ppChainState,
  ScriptContext (ScriptContext, contextRedeemer, contextPurpose, contextTxInfo),
  ScriptPurpose (Spending, Minting, Rewarding, Certifying),
  ScriptTx (ScriptTx, scriptTx, scriptTxPurpose),
  ScriptType (Spend, Mint, Reward, Certify),
  plutusScriptContext,
  scriptTxValid,
) where

import Data.Kind (Type)

import PlutusLedgerApi.V2 qualified as Plutus

import Data.Bifunctor (Bifunctor (second))
import Data.Either (isRight)
import Data.Map (Map)
import Data.Map qualified as Map

import Prettyprinter (Pretty, pretty, vcat)

import Plutus.Model qualified as Model

import Hedgehog.Plutus.TestSingleScript (txRunScript)

type ScriptTx :: ScriptType -> Type
data ScriptTx st = ScriptTx
  { scriptTx :: Model.Tx
  , scriptTxPurpose :: ScriptPurpose st
  }

deriving stock instance Show (ScriptPurpose st) => Show (ScriptTx st)

type ScriptType :: Type
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

type ChainState :: Type
data ChainState = ChainState
  { csMock :: Model.Mock
  , csScripts :: Map Plutus.ScriptHash (Model.Versioned Model.Validator)
  , csMps :: Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy)
  }

ppChainState :: ChainState -> String
ppChainState = show . pretty

instance Pretty ChainState where
  pretty (ChainState mock scripts mps) =
    vcat
      [ "mock    : " <> pretty mock
      , "scripts : " <> pretty (second show <$> Map.toList scripts)
      , "steps   : " <> pretty (second show <$> Map.toList mps)
      ]

scriptTxValid :: forall (st :: ScriptType). ScriptTx st -> Model.Mock -> Bool
scriptTxValid ScriptTx {scriptTx, scriptTxPurpose} m =
  isRight $
    txRunScript
      m
      scriptTx
      (plutusScriptPurpose scriptTxPurpose)

plutusScriptContext ::
  forall (d :: Type) (st :: ScriptType).
  ScriptContext d st ->
  Plutus.ScriptContext
plutusScriptContext
  ScriptContext
    { contextTxInfo = txInfo
    , contextPurpose = sp
    } = Plutus.ScriptContext txInfo (plutusScriptPurpose sp)

plutusScriptPurpose ::
  forall (st :: ScriptType).
  ScriptPurpose st ->
  Plutus.ScriptPurpose
plutusScriptPurpose (Spending ref) = Plutus.Spending ref
plutusScriptPurpose (Minting cs) = Plutus.Minting cs
plutusScriptPurpose (Rewarding sc) = Plutus.Rewarding sc
plutusScriptPurpose (Certifying cert) = Plutus.Certifying cert

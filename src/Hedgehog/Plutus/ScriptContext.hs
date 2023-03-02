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

import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..))
import Control.Monad (forM_, void)
import Data.Map qualified as Map
import Data.Maybe (isJust)
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
scriptTxValid
  ScriptTx
    { scriptTx =
      Model.Tx
        { Model.tx'plutus =
          Tx
            { txInputs
            , txReferenceInputs
            , txSignatures
            , txData
            }
        , Model.tx'extra = _extra
        }
    , scriptTxPurpose
    }
  Model.Mock
    { Model.mockUtxos = utxos
    , Model.mockUsers = users
    , Model.mockDatums = allDatums
    } =
    isJust $ do
      forM_ txInputs validateIn
      forM_ txReferenceInputs validateIn
      forM_ (Map.keys txSignatures) validatePkh
      forM_ (Map.keys txData) validateDh
      case scriptTxPurpose of
        Spending ref -> void $ validateRef ref
        _ -> pure ()
    where
      validateIn TxIn {txInRef} = validateRef txInRef
      validateRef ref = Map.lookup ref utxos
      validatePkh pkh = Map.lookup pkh users
      validateDh dh = Map.lookup dh allDatums

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

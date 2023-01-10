{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.Tx where

import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Interval qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Lattice ((/\))
import UntypedPlutusCore qualified as UPLC

import Plutus.Model qualified as Model

data Balanced = Balanced | Unbalanced

data Tx (bal :: Balanced) = Tx
  { txInputs :: !(Set TxIn)
  , txOutputs :: !(Vector TxOut)
  , txMint ::
      !( Map
          Plutus.CurrencySymbol
          (Map Plutus.TokenName Integer, Plutus.Redeemer)
       )
  , txFee :: !Model.Ada
  , txValidRange :: !Plutus.POSIXTimeRange
  , txExtraSignatures :: !(Set Plutus.PubKeyHash)
  }

data TxIn = TxIn
  { txInRef :: !Plutus.TxOutRef
  , txInScript :: !(Maybe InScript)
  }
  deriving stock (Eq, Ord)

data TxOut = TxOut
  { txOutAddress :: !Plutus.Address
  , txOutValue :: !Plutus.Value
  , txOutDatum :: !(Maybe Plutus.Datum)
  , txOutReferenceScript :: !(Maybe Script)
  }

newtype Script = Script
  { unScript :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
  }

data InScript = InScript
  { inScriptSource :: ScriptSource
  , inScriptData :: Maybe (Plutus.Redeemer, Plutus.Datum)
  }
  deriving stock (Eq, Ord)

data ScriptSource = InTransaction | RefScript !Plutus.TxOutRef
  deriving stock (Eq, Ord)

instance Semigroup (Tx bal) where
  l <> r =
    Tx
      { txInputs = txInputs l <> txInputs r
      , txOutputs = txOutputs l <> txOutputs r
      , txMint = txMint l <> txMint r
      , txFee = txFee l + txFee r
      , txValidRange = txValidRange l /\ txValidRange r
      , txExtraSignatures = txExtraSignatures l <> txExtraSignatures r
      }

instance Monoid (Tx bal) where
  mempty =
    Tx
      { txInputs = Set.empty
      , txOutputs = Vector.empty
      , txMint = Map.empty
      , txFee = 0
      , txValidRange = Plutus.never
      , txExtraSignatures = Set.empty
      }

data TxContext = TxContext
  { mockchain :: !Model.Mock
  , interestingScripts :: !(Map Plutus.ScriptHash Script)
  }

data ScriptPurpose
  = Minting Plutus.CurrencySymbol
  | Spending TxIn

balanceTx :: Model.Mock -> Tx 'Unbalanced -> Maybe (Tx 'Balanced)
balanceTx = _

toSimpleModelTx :: TxContext -> Tx 'Balanced -> Model.Tx
toSimpleModelTx = _

toLedgerScriptPurpose :: ScriptPurpose -> Ledger.ScriptPurpose era
toLedgerScriptPurpose = _

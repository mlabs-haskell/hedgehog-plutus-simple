{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.Tx where

import Control.Monad (forM, guard)
import Control.Monad.State.Strict (gets)
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1 (PubKeyHash, Value)
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V1.Interval qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Lattice ((/\))
import UntypedPlutusCore qualified as UPLC

import Plutus.Model qualified as Model

data Balanced = Balanced | Unbalanced

-- | An idealized transaction type, used throughout @hedgehog-plutus-simple@.
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
  = Minting
      Plutus.CurrencySymbol
      (Map Plutus.TokenName Integer, Plutus.Redeemer)
  | Spending Plutus.TxOutRef InScript

{- | Balance a transaction. Algorithm:

 * Find the smallest set of 'non-special' UTxOs (PubKey locked no datum) that
   cover the transaction balance.

 * Pay change, if any, to a random PubKeyHash.

  In the future, hps should support custom balancers.
-}
balanceTx :: Model.Mock -> Tx 'Unbalanced -> Maybe (Tx 'Balanced)
balanceTx mock tx = do
  let balanceingKeys = Map.keys $ Model.mockUsers mock
  -- It probably doesn't make sense to balance a transaction
  -- as every user at once in most cases
  -- so we should probably make this an argument to balance
  inputs <-
    forM
      (Set.toList $ txInputs tx)
      ((`Map.lookup` Model.mockUtxos mock) . txInRef)
  let valueOut = Vector.foldMap' txOutValue (txOutputs tx)
  let valueIn = foldMap' Plutus.txOutValue inputs
  let extra = Plutus.unionWith (\a b -> max (a - b) 0) valueIn valueOut
  let deficit = Plutus.unionWith (\a b -> max (a - b) 0) valueOut valueIn
  (payDeficet, moreChange) <- tryPayAs mock balanceingKeys deficit
  let change = extra <> moreChange
  pure $
    unsafeBalance $
      tx
        <> payDeficet
        <> mempty
          { txOutputs =
              pure $
                TxOut
                  { txOutAddress = Plutus.pubKeyHashAddress (head balanceingKeys)
                  , -- This head is where we could put in randomness if we
                    -- decide that makes sense
                    txOutValue = change
                  , txOutDatum = Nothing
                  , txOutReferenceScript = Nothing
                  }
          }
  where
    -- TODO this should probably be replaced
    -- with a function that checks the balance and
    -- returns a Maybe
    unsafeBalance :: Tx 'Unbalanced -> Tx 'Balanced
    unsafeBalance = coerce

-- based on `split'` in psm
-- the (b :: Balanced) is polymorphic because
-- while the Tx returned is not balanced it's intended
-- for use in creating a balanced transaction
tryPayAs ::
  Model.Mock ->
  [PubKeyHash] ->
  Value ->
  Maybe (Tx 'Unbalanced, Value)
tryPayAs mock keys val = fst $ (`Model.runMock` mock) $ do
  refs <- concat <$> mapM (Model.txOutRefAt . Plutus.pubKeyHashAddress) keys
  mUtxos <-
    gets
      ( (\m -> mapM (\r -> (r,) <$> Map.lookup r m) refs)
          . Map.filter
            ( \txo ->
                isNothing (Plutus.txOutReferenceScript txo)
                  && (== Plutus.NoOutputDatum) (Plutus.txOutDatum txo)
                  -- && isNothing (Plutus.txOutDatum txo)
            )
          . Model.mockUtxos
      )
  case mUtxos of
    Just utxos -> pure $ toRes $ foldl go (val, mempty, []) utxos
    Nothing -> pure Nothing
  where
    toRes :: (Value, Value, [TxIn]) -> Maybe (Tx 'Unbalanced, Value)
    toRes (rem, change, refs) = do
      guard $ rem == mempty
      pure (mempty {txInputs = Set.fromList refs}, change)

    go ::
      (Value, Value, [TxIn]) ->
      (Plutus.TxOutRef, Plutus.TxOut) ->
      (Value, Value, [TxIn])
    go (remaining, change, refs) (outRef, out)
      | remaining == mempty = (remaining, change, refs)
      | intersect /= mempty =
          (remaining', change', asTxIn : refs)
      | otherwise = (remaining, change, refs)
      where
        refVal = Plutus.txOutValue out
        intersect = Plutus.unionWith min remaining refVal
        remaining' = Plutus.unionWith (-) remaining intersect
        change' =
          Plutus.unionWith (+) change $
            Plutus.unionWith (-) refVal intersect
        asTxIn = TxIn outRef Nothing

{- | Generate a @plutus-simple-model@ 'Model.Tx' from a @hedgehog-plutus-simple@
 'Tx'. This should automatically add neccesary scripts and signatures.
-}
toSimpleModelTx :: TxContext -> Tx 'Balanced -> Model.Tx
toSimpleModelTx = _

-- 'era' can be constrained as neccesary.

{- | Generate a ledger 'Ledger.Tx' from a @hedgehog-plutus-simple@
 'Tx'. This should automatically add neccesary scripts and signatures.
-}
toLedgerTx :: TxContext -> Tx bal -> Ledger.Tx era
toLedgerTx = _

-- | Generate the relevant transaction fragment for a 'ScriptPurpose'
scriptPurposeTx :: TxContext -> ScriptPurpose -> Tx 'Unbalanced
scriptPurposeTx = _

{- | Generate a ledger 'Ledger.ScriptPurpose' from a @hedgehog-plutus-simple@
 'ScriptPurpose'.
-}
toLedgerScriptPurpose :: ScriptPurpose -> Ledger.ScriptPurpose era
toLedgerScriptPurpose = _

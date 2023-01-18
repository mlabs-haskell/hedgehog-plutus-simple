{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.Tx where

import Control.Arrow ((>>>))
import Control.Monad (guard, liftM2)
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Core (TxBody)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as MV
import Cardano.Ledger.Shelley.API.Wallet (evaluateTransactionBalance)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (ScriptHash))

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Cardano.Alonzo qualified as Alonzo
import Plutus.Model.Fork.Cardano.Babbage qualified as Babbage
import Plutus.Model.Fork.Cardano.Class qualified as Class
import Plutus.Model.Mock.ProtocolParameters qualified as Model

import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC

import PlutusLedgerApi.V1 (PubKeyHash, Value, singleton)
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V1.Interval qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Lattice ((/\))

import Hedgehog qualified
import Hedgehog.Gen qualified as Gen

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
  , datums :: !(Map Plutus.DatumHash Plutus.Datum)
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
balanceTx :: TxContext -> Tx 'Unbalanced -> Maybe (Hedgehog.Gen (Tx 'Balanced))
balanceTx context tx = balanceTxWhere context tx (const True) (const True)

-- | as balanceTx but only uses txOuts belonging to a particular PubKeyHash and always sends change to that PubKeyHash
balanceTxAsPubKey :: TxContext -> Tx 'Unbalanced -> PubKeyHash -> Maybe (Hedgehog.Gen (Tx 'Balanced))
balanceTxAsPubKey context tx pkh =
  balanceTxWhere
    context
    tx
    ( Plutus.txOutAddress
        >>> Plutus.addressCredential
        >>> ( \case
                Plutus.PubKeyCredential pkh' -> pkh' == pkh
                _ -> False
            )
    )
    (== pkh)

-- | as balanceTx but with additional predicates about which TxOuts and PubKeyHashs can be used
balanceTxWhere ::
  TxContext ->
  Tx 'Unbalanced ->
  (Plutus.TxOut -> Bool) ->
  (Plutus.PubKeyHash -> Bool) ->
  Maybe (Hedgehog.Gen (Tx 'Balanced))
balanceTxWhere context tx predTxout predPkh = do
  let mock = mockchain context
  (valueIn, valueOut) <- getValueInAndOut context tx
  -- TODO we may want to change this to something like
  -- Either FailReason (Tx 'Balanced)
  -- to distinguish things like failure by tx lookup
  -- and failure by not enough value at keys
  let extra = posDif valueIn valueOut
  let deficit = posDif valueOut valueIn
  let alreadySpending txOut =
        txOut
          `elem` catMaybes
            [ Map.lookup (txInRef txin) (Model.mockUtxos mock)
            | txin <- Set.toList $ txInputs tx
            ]
  spendGen <- spendWhere mock deficit (liftM2 (&&) (not . alreadySpending) predTxout)
  let payoutKeys = filter predPkh $ Map.keys $ Model.mockUsers mock
  guard $ not $ null payoutKeys
  pure $ do
    Spend outRefs moreChange <- spendGen
    payOutKey <- Gen.element payoutKeys
    let change = extra <> moreChange
    let checked =
          confirmBalanced context $
            tx
              <> mempty
                { txInputs =
                    Set.fromList $
                      Vector.toList $
                        (`TxIn` Nothing) <$> outRefs
                , txOutputs =
                    pure $
                      TxOut
                        { txOutAddress = Plutus.pubKeyHashAddress payOutKey
                        , txOutValue = change
                        , txOutDatum = Nothing
                        , txOutReferenceScript = Nothing
                        }
                }
    case checked of
      Just tx -> pure tx
      Nothing -> error "balanceTx produced an unbalanced tx"

getBalance :: TxContext -> Tx b -> Maybe Value
getBalance context tx =
  case protocol of
    Model.AlonzoParams (params :: Core.PParams Alonzo.Era) ->
      fromParams params
    Model.BabbageParams (params :: Core.PParams Babbage.Era) ->
      fromParams params
  where
    -- fromParams ::
    --  forall era.
    --    ( Core.EraTx era
    --    , ShelleyEraTxBody era
    --    , Ledger.Value era ~ MV.MaryValue era
    --    ) =>
    --    Core.PParams era -> Maybe Value
    -- TODO this signature works but breaks the call sites
    fromParams params = do
      Right utxo <- pure $ Class.toUtxo mempty networkId []
      -- TODO what should the utxo be for this?
      pure $
        maryToPlutus $
          evaluateTransactionBalance
            params
            utxo
            (const True) -- TODO is this right?
            body

    networkId :: Network
    networkId = Model.mockConfigNetworkId mockConfig

    protocol :: Model.PParams
    protocol = Model.mockConfigProtocol mockConfig

    mockConfig :: Model.MockConfig
    mockConfig = Model.mockConfig $ mockchain context

    body :: Class.IsCardanoTx era => TxBody era
    body = Class.getTxBody $ toLedgerTx context tx

maryToPlutus :: forall era. MV.MaryValue era -> Plutus.Value
maryToPlutus (MV.MaryValue ada rest) =
  singleton "" "" ada
    <> mconcat
      [ singleton
        (Plutus.currencySymbol $ hashToBytes $ unScriptHash $ MV.policyID pid)
        (Plutus.tokenName $ SBS.fromShort $ MV.assetName tn)
        amt
      | (pid, toks) <- Map.toList rest
      , (tn, amt) <- Map.toList toks
      ]
  where
    unScriptHash (ScriptHash h) = h -- The constructor doesn't have this

getValueInAndOut :: TxContext -> Tx bal -> Maybe (Value, Value)
getValueInAndOut context tx = do
  val <- getBalance context tx
  pure (posDif val mempty, posDif mempty val)

posDif :: Plutus.Value -> Plutus.Value -> Plutus.Value
posDif = Plutus.unionWith (fmap (max 0) . (-))

confirmBalanced :: TxContext -> Tx 'Unbalanced -> Maybe (Tx 'Balanced)
confirmBalanced context tx =
  (getBalance context tx >>= guard . (== mempty)) $> coerce tx

data Spend = Spend
  { spendUtxos :: Vector Plutus.TxOutRef
  , excess :: Plutus.Value
  }

{- | Try to satisfy a value from available 'non-special' UTxOs.

A UTxO is 'non-special' iff:

* It is locked by a PubKey

* It does not hold a datum

The refs need not be held by the same PubKey.
-}
spendWhere ::
  Model.Mock ->
  Plutus.Value ->
  (Plutus.TxOut -> Bool) ->
  Maybe (Hedgehog.Gen Spend)
spendWhere mock val pred = do
  let utxos = Map.toList $ Map.filter (liftM2 (&&) nonSpecial pred) $ Model.mockUtxos mock
  guard $ not $ null utxos
  detSpend <- toRes $ foldl go (val, Spend Vector.empty mempty) utxos
  -- create a deterministic spend to test that balance is possible
  Just $ do
    newUtxos <- Gen.shuffle utxos
    let spend = toRes $ foldl go (val, Spend Vector.empty mempty) newUtxos
    case spend of
      Nothing -> pure detSpend
      -- if the shuffled balance somehow fails
      -- just return the deterministic spend
      Just s -> pure s
  where
    nonSpecial :: Plutus.TxOut -> Bool
    nonSpecial txout =
      let
        cred = Plutus.addressCredential $ Plutus.txOutAddress txout
        noDatum = Plutus.txOutDatum txout == Plutus.NoOutputDatum
        isPubKey = case cred of
          Plutus.PubKeyCredential _ -> True
          _ -> False
       in
        isPubKey && noDatum

    toRes :: (Value, Spend) -> Maybe Spend
    toRes (rem, s) = guard (rem == mempty) $> s

    go ::
      (Value, Spend) ->
      (Plutus.TxOutRef, Plutus.TxOut) ->
      (Value, Spend)
    go (remaining, s@(Spend refs change)) (outRef, out)
      | remaining == mempty = (remaining, s)
      | intersect /= mempty =
          (remaining', Spend (Vector.cons outRef refs) change')
      | otherwise = (remaining, s)
      where
        refVal = Plutus.txOutValue out
        intersect = Plutus.unionWith min remaining refVal
        remaining' = posDif remaining intersect
        change' = change <> posDif refVal intersect

{- | Generate a @plutus-simple-model@ 'Model.Tx' from a @hedgehog-plutus-simple@
 'Tx'. This should automatically add neccesary scripts and signatures.
-}
toSimpleModelTx :: TxContext -> Tx bal -> Model.Tx
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

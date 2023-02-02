{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.Tx (
  Tx (..),
  Balanced (..),
  TxIn (..),
  TxOut (..),
  Script (..),
  InScript (..),
  ScriptSource (..),
  TxContext (..),
  ScriptPurpose (..),
  balanceTx,
  balanceTxAsPubKey,
  balanceTxWhere,
  confirmBalanced,
  Spend (..),
  spendWhere,
  toSimpleModelTx,
  toLedgerTx,
  scriptPurposeTx,
  toLedgerScriptPurpose,
  CoreTx (..),
  convertScript,
)
where

import Control.Arrow (first, (>>>))
import Control.Monad (guard, liftM2)

import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Cardano.Crypto.Hash.Class (hashFromBytes, hashToBytes)
import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.BaseTypes (txIxFromIntegral)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Crypto (ADDRHASH, HASH, StandardCrypto)
import Cardano.Ledger.Mary.Value qualified as MV
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.Shelley.API.Wallet (CLI, evaluateTransactionBalance)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (ScriptHash))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)
import Cardano.Ledger.TxIn qualified as Ledger

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Cardano.Alonzo qualified as Alonzo
import Plutus.Model.Fork.Cardano.Babbage qualified as Babbage
import Plutus.Model.Fork.Cardano.Class qualified as Class
import Plutus.Model.Fork.Ledger.Scripts (scriptHash)
import Plutus.Model.Fork.Ledger.TimeSlot qualified as Time
import Plutus.Model.Fork.Ledger.Tx qualified as Fork
import Plutus.Model.Fork.PlutusLedgerApi.V1.Scripts qualified as Scripts
import Plutus.Model.Mock.ProtocolParameters qualified as Model

import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC

import PlutusLedgerApi.V1 (PubKeyHash, Value, singleton)
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V1.Interval qualified as Plutus
import PlutusLedgerApi.V1.Tx (RedeemerPtr (RedeemerPtr), ScriptTag (Mint))
import PlutusLedgerApi.V1.Value qualified as Plutus
import PlutusLedgerApi.V2 (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
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
getBalance
  context@TxContext
    { mockchain =
      Model.Mock
        { Model.mockUtxos = utxos
        , Model.mockConfig =
          Model.MockConfig
            { Model.mockConfigNetworkId = networkId
            , Model.mockConfigProtocol = protocol
            }
        }
    , interestingScripts
    }
  tx =
    case (protocol, toLedgerTx context tx) of
      (Model.AlonzoParams params, Alonzo coreTx) ->
        go @Alonzo.Era params coreTx
      (Model.BabbageParams params, Babbage coreTx) ->
        go @Babbage.Era params coreTx
      -- this error should be unreachable
      _ -> error "era mismatch between balance and toLedgerTx"
    where
      go ::
        forall era.
        ( Core.EraTx era
        , Class.IsCardanoTx era
        , CLI era
        , ShelleyEraTxBody era
        , Core.Value era ~ MV.MaryValue StandardCrypto
        ) =>
        Core.PParams era ->
        Core.Tx era ->
        Maybe Value
      go params coreTx = do
        let utxoMap = first (`Fork.TxIn` Nothing) <$> Map.toList utxos
        -- TODO is this nothing right
        Right utxo <- pure $ Class.toUtxo (Map.map convertScript interestingScripts) networkId utxoMap
        pure $
          maryToPlutus $
            evaluateTransactionBalance @era
              params
              utxo
              (const True) -- TODO is this right?
              (Class.getTxBody coreTx)

maryToPlutus :: MV.MaryValue StandardCrypto -> Plutus.Value
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

{- | Try to satisfy a value from available 'non-special' UTxOs matching the provided predicate.

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
toSimpleModelTx
  TxContext
    { interestingScripts
    , mockchain
    , datums
    }
  Tx
    { txInputs
    , txOutputs
    , txMint
    , txFee
    , txValidRange
    , txExtraSignatures
    } =
    Model.Tx
      { Model.tx'extra = mempty
      , Model.tx'plutus =
          mempty
            { Fork.txInputs = Set.map convertTxIn txInputs
            , Fork.txCollateral = mempty
            , -- TODO our tx type should probably support reference inputs
              Fork.txReferenceInputs = mempty
            , Fork.txOutputs = outs
            , Fork.txCollateralReturn = Nothing
            , Fork.txTotalCollateral = Nothing
            , Fork.txMint =
                mconcat
                  [ singleton cs tn amt
                  | (cs, (toks, _reds)) <- Map.toList txMint
                  , (tn, amt) <- Map.toList toks
                  ]
            , Fork.txFee = txFee
            , Fork.txValidRange =
                Time.posixTimeRangeToContainedSlotRange
                  (Model.mockConfigSlotConfig $ Model.mockConfig mockchain)
                  txValidRange
            , Fork.txMintScripts =
                Set.fromList $
                  [ scriptToMP $ getScript $ Plutus.ScriptHash cs
                  | (Plutus.CurrencySymbol cs, _) <- Map.toList txMint
                  ]
            , Fork.txSignatures =
                Map.fromList
                  [ (pkh, Model.userSignKey $ getUser pkh)
                  | pkh <- Set.toList txExtraSignatures
                  -- TODO if these are "extra" are the non-extra just
                  -- the ones required by the inputs?
                  -- If so add them here
                  ]
            , Fork.txRedeemers =
                Map.fromList $
                  zip
                    [RedeemerPtr Mint i | i <- [0 ..]] -- TODO is this correct?
                    [red | (_, (_, red)) <- Map.toList txMint]
            , Fork.txData = txData
            , Fork.txScripts = Map.fromList $ zip hashes scripts
            }
      }
    where
      getScript :: Plutus.ScriptHash -> Script
      getScript sh =
        Map.lookup sh interestingScripts
          & fromMaybe (error "script lookup failed")

      getUser :: PubKeyHash -> Model.User
      getUser pkh =
        Map.lookup pkh (Model.mockUsers mockchain)
          & fromMaybe (error "user lookup failed")

      getUTxO :: Plutus.TxOutRef -> Plutus.TxOut
      getUTxO ref =
        Map.lookup ref (Model.mockUtxos mockchain)
          & fromMaybe (error "utxo lookup failed")

      outs' :: [(Plutus.TxOut, Maybe (Plutus.DatumHash, Plutus.Datum))]
      outs' = convertTxOut <$> Vector.toList txOutputs

      outs :: [Plutus.TxOut]
      outs = fst <$> outs'

      txData :: Map Plutus.DatumHash Plutus.Datum
      txData = datums <> Map.fromList (mapMaybe snd outs')
      -- Context datums with any additional datums
      -- from txout conversions

      scripts :: [Model.Versioned Model.Script]
      scripts =
        [ convertScript s
        | (sh, s) <- Map.toList interestingScripts
        , -- filter only the scripts being invoked
        -- by checking their address coresponds to some input
        Plutus.scriptHashAddress sh
          `elem` [ Plutus.txOutAddress $ getUTxO $ txInRef txIn
                 | txIn <- Set.toList txInputs
                 ]
        ]

      hashes :: [Plutus.ScriptHash]
      hashes = scriptHash <$> scripts

      inlineDatums :: Bool
      inlineDatums = True
      -- Maybe this could be computed from version or added to context?
      -- It would not be hard to make it a predicate either
      -- Or just have our TxOut type know how the datum is stored

      -- convert tx out to Plutus.TxOut maybe adding a datum table entry
      convertTxOut ::
        TxOut -> (Plutus.TxOut, Maybe (Plutus.DatumHash, Plutus.Datum))
      convertTxOut
        TxOut
          { txOutAddress
          , txOutValue
          , txOutDatum
          , txOutReferenceScript
          } =
          ( Plutus.TxOut
              { Plutus.txOutValue = txOutValue
              , Plutus.txOutAddress = txOutAddress
              , Plutus.txOutDatum =
                  case txOutDatum of
                    Nothing -> NoOutputDatum
                    Just datum ->
                      if inlineDatums
                        then OutputDatum datum
                        else OutputDatumHash $ Model.datumHash datum
              , Plutus.txOutReferenceScript =
                  scriptHash . convertScript
                    <$> txOutReferenceScript
              }
          , do
              datum <- txOutDatum
              guard $ not inlineDatums
              pure (Model.datumHash datum, datum)
          )

      convertTxIn :: TxIn -> Fork.TxIn
      convertTxIn TxIn {txInRef, txInScript} =
        Fork.TxIn
          { Fork.txInRef = txInRef
          , Fork.txInType =
              let Plutus.TxOut {Plutus.txOutAddress = Plutus.Address cred _} =
                    getUTxO txInRef
               in case cred of
                    Plutus.PubKeyCredential _ -> pure Fork.ConsumePublicKeyAddress
                    Plutus.ScriptCredential sh -> do
                      InScript {inScriptData} <- txInScript
                      (redeemer, datum) <- inScriptData
                      let script = Just $ scriptToVal $ getScript sh
                      pure $ Fork.ConsumeScriptAddress script redeemer datum
          }

-- TODO toV1 is a placeholder
-- our script type should probably know the version
convertScript :: Script -> Model.Versioned Model.Script
convertScript (Script s) = Model.toV1 $ Scripts.Script s

scriptToMP :: Script -> Model.Versioned Model.MintingPolicy
scriptToMP = coerce . convertScript

scriptToVal :: Script -> Model.Versioned Model.Validator
scriptToVal = coerce . convertScript

{- | Generate a ledger 'Ledger.Tx' from a @hedgehog-plutus-simple@
 'Tx'. This should automatically add neccesary scripts and signatures.
-}
toLedgerTx :: TxContext -> Tx bal -> CoreTx
toLedgerTx context tx =
  case Model.mockConfigProtocol $ Model.mockConfig (mockchain context) of
    Model.AlonzoParams (params :: Core.PParams Alonzo.Era) ->
      Alonzo $ cont params
    Model.BabbageParams (params :: Core.PParams Babbage.Era) ->
      Babbage $ cont params
  where
    cont :: Class.IsCardanoTx era => Core.PParams era -> Core.Tx era
    cont params =
      Class.toCardanoTx
        (Map.map convertScript $ interestingScripts context)
        (Model.mockConfigNetworkId $ Model.mockConfig $ mockchain context)
        params
        (toSimpleModelTx context tx)
        & \case
          Left e -> error ("toLedgerTx failed with:" <> show e)
          Right tx -> tx

-- Since toLedgerTx can't actually be polymorphic
-- as the era is forced by the mockchain
-- it needs to return something like this
data CoreTx
  = Alonzo (Core.Tx Alonzo.Era)
  | Babbage (Core.Tx Babbage.Era)

-- | Generate the relevant transaction fragment for a 'ScriptPurpose'
scriptPurposeTx :: ScriptPurpose -> Tx 'Unbalanced
scriptPurposeTx = \case
  Spending ref inscript ->
    mempty
      { txInputs = Set.singleton $ TxIn ref (Just inscript)
      }
  Minting cs (toks, red) ->
    mempty
      { txMint = Map.singleton cs (toks, red)
      }

{- | Generate a ledger 'Ledger.ScriptPurpose' from a @hedgehog-plutus-simple@
 'ScriptPurpose'.
-}
toLedgerScriptPurpose ::
  forall era.
  (Hash.HashAlgorithm (ADDRHASH era)) =>
  (Hash.HashAlgorithm (HASH era)) =>
  ScriptPurpose ->
  Ledger.ScriptPurpose era
toLedgerScriptPurpose = \case
  Spending (Plutus.TxOutRef (Plutus.TxId txIdHash) idx) _inscript ->
    Ledger.Spending $
      Ledger.TxIn
        ( Ledger.TxId $
            SafeHash.unsafeMakeSafeHash $
              fromMaybe (error "failed to repack script hash") $
                hashFromBytes $
                  Plutus.fromBuiltin txIdHash
        )
        (fromMaybe (error "index overflow") $ txIxFromIntegral idx)
  -- The Maybe is because the integer might not fit in a Word64
  -- This shouldn't come up in practice
  Minting (Plutus.CurrencySymbol sh) _ ->
    Ledger.Minting $
      MV.PolicyID $
        ScriptHash $
          fromMaybe (error "failed to repack currencySymbol to hash") $
            hashFromBytes (Plutus.fromBuiltin sh)

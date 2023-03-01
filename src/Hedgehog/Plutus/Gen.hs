{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.Gen where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Vector qualified as Vector

import Control.Monad (replicateM, when)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (MonadState (get), State, evalState, modify)

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.SafeHash qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus

import Cardano.Simple.Cardano.Common qualified as Simple
import Cardano.Simple.Ledger.Slot qualified as Simple
import Plutus.Model qualified as Model
import Plutus.Model.Stake qualified as Model

import Hedgehog (
  Gen,
  MonadGen,
 )
import Hedgehog.Gen (
  bytes,
  choice,
  element,
  hexit,
  integral,
 )
import Hedgehog.Gen qualified as Gen

import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Plutus.Model (Mock)
import PlutusLedgerApi.V1 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName (TokenName),
  TxOutRef,
  toBuiltin,
 )

import GHC.Natural (Natural)
import Hedgehog.Plutus.TestData (EitherOr, Good, ShouldBeNatural, TestData (generalise, validate))
import Hedgehog.Range qualified as Range

data User m = User
  { user :: !Model.User
  , userName :: !String
  , userOutputs :: !(m [Output])
  }

data Output = Output
  { outputRef :: !Plutus.TxOutRef
  , output :: !Plutus.TxOut
  , outputDatum :: !(Maybe Plutus.Datum)
  }

initMockState ::
  forall m.
  (Monad m, Traversable m) =>
  Map String (Plutus.PubKeyHash -> m [(Plutus.TxOut, Maybe Plutus.Datum)]) ->
  Map Plutus.ScriptHash (String, m [(Plutus.TxOut, Maybe Plutus.Datum)]) ->
  Model.MockConfig ->
  m Model.Mock
initMockState users scripts cfg = (`evalState` 0) $ do
  us <- users'
  ss <- scripts'
  let userOutputs = fmap concat . mapM (.userOutputs) . Map.elems $ us
  let scriptOutputs = fmap concat . mapM snd . Map.elems $ ss
  let outputs = concat <$> sequence [userOutputs, scriptOutputs]
  pure $ do
    os <- outputs
    addrs <-
      fmap (Map.mapKeysMonotonic Plutus.pubKeyHashAddress)
        . traverse (fmap (Set.fromList . fmap (.outputRef)) . (.userOutputs))
        $ us
    pure $
      Model.Mock
        { mockUsers = fmap (.user) us
        , mockUtxos =
            Map.fromList
              . fmap (\Output {outputRef, output} -> (outputRef, output))
              $ os
        , mockDatums =
            Map.fromList
              . fmap (\d -> (Model.datumHash d, d))
              $ mapMaybe (.outputDatum) os
        , mockAddresses = addrs
        , mockStake = initStake
        , mockTxs = mempty
        , mockConfig = cfg
        , mockCurrentSlot = Simple.Slot 1
        , mockUserStep = fromIntegral $ Map.size users
        , mockFails = mempty
        , mockInfo = mempty
        , mustFailLog = mempty
        , mockNames =
            Model.MockNames
              { mockNameUsers = fmap (.userName) us
              , mockNameAddresses =
                  Map.mapKeysMonotonic Plutus.scriptHashAddress $
                    fmap fst ss
              , mockNameAssetClasses = Map.empty
              , mockNameCurrencySymbols = Map.empty
              , mockNameTxns = Map.empty
              }
        }
  where
    mkOutputs ::
      m [(Plutus.TxOut, Maybe Plutus.Datum)] ->
      State Integer (m [Output])
    mkOutputs = traverse . traverse $ \(o, d) -> do
      i <- get
      modify (+ 1)
      pure
        Output
          { outputRef = Plutus.TxOutRef genesisTxId i
          , output = o
          , outputDatum = d
          }

    users' :: State Integer (Map Plutus.PubKeyHash (User m))
    users' =
      fmap Map.fromList
        . traverse
          ( \(i, (name, os)) ->
              do
                let user = Model.intToUser i
                    pkh = Model.userPubKeyHash user
                outputs <- mkOutputs (os pkh)
                pure
                  ( pkh
                  , User
                      { user = user
                      , userName = name
                      , userOutputs = outputs
                      }
                  )
          )
        . zip [0 ..]
        . Map.toList
        $ users

    scripts' :: State Integer (Map Plutus.ScriptHash (String, m [Output]))
    scripts' = traverse (traverse mkOutputs) scripts

    genesisTxId :: Plutus.TxId
    genesisTxId =
      Simple.fromTxId
        . Ledger.TxId
        . Ledger.unsafeMakeSafeHash
        . Crypto.castHash
        $ Crypto.hashWith CBOR.serialize' ()

    initStake =
      Model.Stake
        { stake'pools = Map.empty
        , stake'poolIds = Vector.empty
        , stake'stakes = Map.empty
        , stake'nextReward = 0
        }

newtype GenContext a
  = GenContext (ReaderT Mock Gen a)
  deriving newtype (Functor, Applicative, Monad, MonadGen, MonadReader Mock)

runCtx :: Mock -> GenContext a -> Gen a
runCtx m (GenContext gen) = runReaderT gen m

genGoodEither :: (TestData good, MonadGen m) => m good -> m (EitherOr bad good)
genGoodEither g = generalise <$> genValid g

genPositive :: GenContext (ShouldBeNatural Integer)
genPositive =
  generalise
    <$> integral @GenContext @Natural (Range.linear 0 1_000_000)

genTxOutRef :: GenContext TxOutRef
genTxOutRef = do
  utxos <- asks Model.mockUtxos
  when (null utxos) $ error "genTxOutRef couldn't find any out refs"
  element $ Map.keys utxos

genValid ::
  forall a m.
  (MonadGen m, TestData a) =>
  m a ->
  m (Good a)
genValid g = genFromMaybe $ validate @a <$> g

genFromMaybe :: MonadGen m => m (Maybe a) -> m a
genFromMaybe g =
  fromMaybe (error "isJust Nothing was True")
    <$> Gen.filterT isJust g

genTime :: GenContext POSIXTime
genTime = do
  slotCfg <- asks (Model.mockConfigSlotConfig . Model.mockConfig)
  let start = Time.scSlotZeroTime slotCfg
  let len = Time.scSlotLength slotCfg
  i <- integral $ Range.linear 0 1_000_000
  pure $ start + i * fromIntegral len

genValidUser :: GenContext PubKeyHash
genValidUser = do
  users <- asks Model.mockUsers
  element (Map.keys users)

genPubKey :: GenContext PubKeyHash
genPubKey = do
  choice
    [ genValidUser
    , fromHexString 28
    ]

genCS :: GenContext CurrencySymbol
genCS = choice [pure "", fromHexString 28]

genTN :: GenContext TokenName
genTN = TokenName . toBuiltin <$> bytes (Range.linear 0 32)

fromHexString :: (IsString b, MonadGen m) => Int -> m b
fromHexString n = fromString <$> replicateM (2 * n) hexit

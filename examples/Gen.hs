{-# LANGUAGE UndecidableInstances #-}

module Gen where

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

import AuctionExample (
  Auction (Auction),
  AuctionDatum (AuctionDatum),
  AuctionTest (AuctionTest),
  AuctionTestRedeemer (TestRedeemerBid),
  Bid (Bid),
  SelfOutput (SelfOutput),
 )

import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Plutus.Model (Mock)
import Plutus.Model qualified as Model
import PlutusLedgerApi.V1 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName (TokenName),
  TxOutRef,
  toBuiltin,
 )

import Control.Monad (replicateM, when)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Reader.Class (asks)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (fromString))
import GHC.Natural (Natural)
import Hedgehog.Plutus.TestData (EitherOr, Good, ShouldBeNatural, TestData (generalise, validate))
import Hedgehog.Range qualified as Range

newtype GenContext a
  = GenContext (ReaderT Mock Gen a)
  deriving newtype (Functor, Applicative, Monad, MonadGen, MonadReader Mock)

runCtx :: Mock -> GenContext a -> Gen a
runCtx m (GenContext gen) = runReaderT gen m

genGoodAuctionTest :: GenContext (Good AuctionTest)
genGoodAuctionTest =
  genValid @AuctionTest $
    AuctionTest
      <$> genTxOutRef
      <*> pure (generalise ())
      <*> genRed

genRed :: GenContext AuctionTestRedeemer
genRed =
  TestRedeemerBid
    <$> genValidUser
    <*> genPositive
    <*> genGoodEither (pure $ SelfOutput (generalise ()) (generalise ()))
    <*> genGoodEither (pure $ generalise ())

genGoodEither :: (TestData good, MonadGen m) => m good -> m (EitherOr bad good)
genGoodEither g = do
  good <- genValid g
  pure $ generalise good

genPositive :: GenContext (ShouldBeNatural Integer)
genPositive = generalise <$> integral @GenContext @Natural (Range.linear 0 1_000_000)

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

genAuctionDatum :: GenContext AuctionDatum
genAuctionDatum =
  AuctionDatum
    <$> genAuction
    <*> choice [pure Nothing, Just <$> genBid]

genBid :: GenContext Bid
genBid = Bid <$> genPubKey <*> bidAmt

genAuction :: GenContext Auction
genAuction =
  Auction
    <$> genPubKey
    <*> genTime
    <*> bidAmt
    -- TODO should we generate negative min bids
    <*> genCS
    <*> genTN

bidAmt :: GenContext Integer
bidAmt = integral $ Range.linear 1 1_000_000

genTime :: GenContext POSIXTime
genTime = do
  slotCfg <- asks (Model.mockConfigSlotConfig . Model.mockConfig)
  let start = Time.scSlotZeroTime slotCfg
  let len = Time.scSlotLength slotCfg
  i <- integral $ Range.linear 0 1_000_000
  pure $ start + i * fromIntegral len

-- TODO some of this is probably worth exposeing in hps

genValidUser :: GenContext PubKeyHash
genValidUser = do
  users <- asks Model.mockUsers
  element (Map.keys users)

genPubKey :: GenContext PubKeyHash
genPubKey = do
  choice
    [ genValidUser
    , fromHexString 28 -- TODO check this is the right length
    ]

genCS :: GenContext CurrencySymbol
genCS = fromHexString 32 -- TODO add choice for lookup from mps after brian/script-context is merged

genTN :: GenContext TokenName
genTN = TokenName . toBuiltin <$> bytes (Range.linear 0 28)

fromHexString :: (IsString b, MonadGen m) => Int -> m b
fromHexString n = fromString <$> replicateM (2 * n) hexit

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

import AuctionExample (
  Auction (Auction),
  AuctionDatum (AuctionDatum),
  Bid (Bid),
 )

import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Plutus.Model (Mock)
import Plutus.Model qualified as Model
import PlutusLedgerApi.V1 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName (TokenName),
  toBuiltin,
 )

import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Reader.Class (asks)
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import Hedgehog.Range qualified as Range

newtype GenContext a
  = GenContext (ReaderT Mock Gen a)
  deriving newtype (Functor, Applicative, Monad, MonadGen, MonadReader Mock)

runCtx :: Mock -> GenContext a -> Gen a
runCtx m (GenContext gen) = runReaderT gen m

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
bidAmt = integral $ Range.linear (negate 1_000_000) 1_000_000

genTime :: GenContext POSIXTime
genTime = do
  slotCfg <- asks (Model.mockConfigSlotConfig . Model.mockConfig)
  let start = Time.scSlotZeroTime slotCfg
  let len = Time.scSlotLength slotCfg
  i <- integral $ Range.linear 0 1_000_000
  pure $ start + i * fromIntegral len

-- TODO some of this is probably worth exposeing in hps

genPubKey :: GenContext PubKeyHash
genPubKey = do
  choice
    [ do
        users <- asks Model.mockUsers
        element (Map.keys users)
    , fromHexString 28 -- TODO check this is the right length
    ]

genCS :: GenContext CurrencySymbol
genCS = fromHexString 32 -- TODO add choice for lookup from mps after brian/script-context is merged

genTN :: GenContext TokenName
genTN = TokenName . toBuiltin <$> bytes (Range.linear 0 28)

fromHexString :: (IsString b, MonadGen m) => Int -> m b
fromHexString n = fromString <$> replicateM (2 * n) hexit

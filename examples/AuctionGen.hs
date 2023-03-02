{-# LANGUAGE UndecidableInstances #-}

module AuctionGen where

import Hedgehog.Gen (
  choice,
  integral,
 )

import Hedgehog.Plutus.Gen

import AuctionExample (
  Auction (Auction),
  AuctionDatum (AuctionDatum),
  AuctionTest (AuctionTest),
  AuctionTestRedeemer (TestRedeemerBid),
  Bid (Bid),
  SelfOutput (SelfOutput),
 )

import Hedgehog.Plutus.TestData (Good, TestData (generalise))
import Hedgehog.Range qualified as Range

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

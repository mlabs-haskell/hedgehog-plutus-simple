module Spec where

import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog.Plutus.TxTest (
  txTestBad,
  txTestBadAdjunction,
  txTestGood,
  txTestGoodAdjunction,
 )

import AuctionExample (auctionTest)

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel
        $ Hedgehog.Group
          "Auction example tests"
        $ take
          1
          [ ("good data adjuncts for bid", goodBidAdjunction)
          , ("good data adjuncts for close", goodCloseAdjunction)
          , ("bad data adjuncts for bid", badBidAdjunction)
          , ("bad data adjuncts for close", badCloseAdjunction)
          , ("good data succeeds for bid", goodBidScript)
          , ("good data succeeds for close", goodCloseScript)
          , ("bad data fails for bid", badBidScript)
          , ("bad data fails for close", badCloseScript)
          ]
    ]

goodBidAdjunction :: Hedgehog.Property
goodBidAdjunction = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGoodAdjunction auctionTest initialState datum good

goodCloseAdjunction :: Hedgehog.Property
goodCloseAdjunction = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGoodAdjunction auctionTest initialState datum good

badBidAdjunction :: Hedgehog.Property
badBidAdjunction = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBadAdjunction auctionTest initialState datum bad

badCloseAdjunction :: Hedgehog.Property
badCloseAdjunction = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBadAdjunction auctionTest initialState datum bad

goodBidScript :: Hedgehog.Property
goodBidScript = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGood auctionTest initialState datum good

goodCloseScript :: Hedgehog.Property
goodCloseScript = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGood auctionTest initialState datum good

badBidScript :: Hedgehog.Property
badBidScript = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBad auctionTest initialState datum bad

badCloseScript :: Hedgehog.Property
badCloseScript = Hedgehog.property $ do
  initialState <- _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBad auctionTest initialState datum bad

nft :: Plutus.Value
nft = Plutus.singleton (Plutus.CurrencySymbol "FFFF") "NFT" 1

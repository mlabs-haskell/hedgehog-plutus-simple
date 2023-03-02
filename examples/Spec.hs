import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import Plutus.Model qualified as Model

import Hedgehog.Plutus.TxTest (txTestGoodAdjunction)

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
          , ("good data adjuncts for close", _)
          , ("bad data adjuncts for bid", _)
          , ("bad data adjuncts for close", _)
          , ("good data succeeds for bid", _)
          , ("good data succeeds for close", _)
          , ("bad data fails for bid", _)
          , ("bad data fails for close", _)
          ]
    ]

-- goodAdjunction :: Hedgehog.Property
-- goodAdjunction = Hedgehog.property $ do
--   init <- Hedgehog.forAllWith Model.ppMock _
--   datum <- Hedgehog.forAll _
--   good <- Hedgehog.forAll _
--   txTestGoodAdjunction auctionTest init datum good

goodBidAdjunction :: Hedgehog.Property
goodBidAdjunction = Hedgehog.property $ do
  init <- Hedgehog.forAllWith Model.ppMock $ do
    pure $
      snd $
        Model.runMock
          ( do
              seller <- Model.newUser (lovelaceValue 1 <> nft) -- seller
              Model.newUser mempty -- old bidder
              Model.newUser (lovelaceValue 100) -- new bidder
              spend <- Model.spend seller (lovelaceValue 1 <> nft)
              Model.submitTx
                seller
                ( Model.userSpend spend
                    <> Model.payToScript _ _ (lovelaceValue 1 <> nft)
                )
          )
          (Model.initMock Model.defaultAlonzo (lovelaceValue 101 <> nft))
  txTestGoodAdjunction auctionTest init _ _

lovelaceValue :: Integer -> Plutus.Value
lovelaceValue = Plutus.singleton Plutus.adaSymbol Plutus.adaToken

nft :: Plutus.Value
nft = Plutus.singleton (Plutus.CurrencySymbol "FFFF") "NFT" 1

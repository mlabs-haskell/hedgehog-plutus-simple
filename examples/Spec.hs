import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import AuctionGen (genAuctionDatum, genGoodAuctionTest)
import Hedgehog.Plutus.Gen (runCtx)
import Plutus.Model qualified as Model
import Plutus.Model.Mock (initMock)

import Hedgehog.Plutus.TxTest (ChainState (..), txTestGoodAdjunction)

import AuctionExample (auctionTest)
import Plutus.Model.V2 (defaultBabbage)
import PlutusLedgerApi.V1.Value qualified as Value

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel
        $ Hedgehog.Group
          "Auction example tests"
        $ take
          1 -- TODO remove when more tests run
          [ ("goodAdjunction", goodAdjunction)
          , -- , ("good data adjuncts for bid", goodBidAdjunction)
            ("good data adjuncts for close", _)
          , ("bad data adjuncts for bid", _)
          , ("bad data adjuncts for close", _)
          , ("good data succeeds for bid", _)
          , ("good data succeeds for close", _)
          , ("bad data fails for bid", _)
          , ("bad data fails for close", _)
          ]
    ]

goodAdjunction :: Hedgehog.Property
goodAdjunction = Hedgehog.property $ do
  initialState <-
    Hedgehog.forAllWith
      Model.ppMock
      (pure $ initMock defaultBabbage (Value.singleton "" "" 1_000_000))
  datum <- Hedgehog.forAll $ runCtx initialState genAuctionDatum
  good <- Hedgehog.forAll $ runCtx initialState genGoodAuctionTest
  let chainState = ChainState initialState mempty mempty
  txTestGoodAdjunction auctionTest chainState datum good

-- goodBidAdjunction :: Hedgehog.Property
-- goodBidAdjunction = Hedgehog.property $ do
--  init <-
--    Hedgehog.forAllWith Model.ppMock $
--      element $
--        initMockState
--          mempty -- TODO generate users map
--          mempty -- TODO generate scripts map
--          Model.defaultAlonzo
--  mock2 <- Hedgehog.forAllWith Model.ppMock $ do
--    pure $
--      snd $
--        Model.runMock
--          ( do
--              seller <- Model.newUser (lovelaceValue 1 <> nft) -- seller
--              Model.newUser mempty -- old bidder
--              Model.newUser (lovelaceValue 100) -- new bidder
--              spend <- Model.spend seller (lovelaceValue 1 <> nft)
--              Model.submitTx
--                seller
--                ( Model.userSpend spend
--                    <> Model.payToScript _ _ (lovelaceValue 1 <> nft)
--                )
--          )
--          init
--  txTestGoodAdjunction auctionTest (_ mock2) _ _

_lovelaceValue :: Integer -> Plutus.Value
_lovelaceValue = Plutus.singleton Plutus.adaSymbol Plutus.adaToken

_nft :: Plutus.Value
_nft = Plutus.singleton (Plutus.CurrencySymbol "FFFF") "NFT" 1

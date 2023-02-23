import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import Gen (genAuctionDatum, genGoodAuctionTest, runCtx)
import Plutus.Model.Mock (initMock)
import Plutus.Model.Pretty qualified as Model

import Hedgehog.Plutus.TxTest (
  txTestBad,
  txTestBadAdjunction,
  txTestGood,
  txTestGoodAdjunction,
 )

import AuctionExample (auctionTest)
import Plutus.Model.V2 (defaultBabbage)
import PlutusLedgerApi.V1.Value qualified as Value

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel
        $ Hedgehog.Group
          "Auction example tests"
        $ drop
          4
          [ ("good data adjuncts", goodAdjunction)
          , ("bad data adjuncts", badAdjunction)
          , ("good data succeeds", goodScript)
          , ("bad data fails", badScript)
          ]
    ]

goodAdjunction :: Hedgehog.Property
goodAdjunction = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith Model.ppMock (pure $ initMock defaultBabbage (Value.singleton "" "" 1_000_000))
  datum <- Hedgehog.forAll $ runCtx initialState genAuctionDatum
  good <- Hedgehog.forAll $ runCtx initialState genGoodAuctionTest
  txTestGoodAdjunction auctionTest initialState datum good

badAdjunction :: Hedgehog.Property
badAdjunction = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith Model.ppMock _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBadAdjunction auctionTest initialState datum bad

goodScript :: Hedgehog.Property
goodScript = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith Model.ppMock _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGood auctionTest initialState datum good

badScript :: Hedgehog.Property
badScript = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith Model.ppMock _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBad auctionTest initialState datum bad

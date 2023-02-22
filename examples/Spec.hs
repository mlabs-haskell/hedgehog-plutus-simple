import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import Plutus.Model.Pretty qualified as Model

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
    [ Hedgehog.checkParallel $
        Hedgehog.Group
          "Auction example tests"
          [ ("good data adjuncts", goodAdjunction)
          , ("bad data adjuncts", badAdjunction)
          , ("good data succeeds", goodScript)
          , ("bad data fails", badScript)
          ]
    ]

goodAdjunction :: Hedgehog.Property
goodAdjunction = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith Model.ppMock _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
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

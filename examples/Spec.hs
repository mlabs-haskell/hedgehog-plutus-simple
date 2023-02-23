import Hedgehog qualified
import Hedgehog.Main qualified as Hedgehog

import Gen (genAuctionDatum, genGoodAuctionTest, runCtx)
import Plutus.Model.Mock (initMock)

import Hedgehog.Plutus.TxTest (
  ChainState (ChainState, csMock),
  ppChainState,
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
        $ take
          1
          [ ("good data adjuncts", goodAdjunction)
          , ("bad data adjuncts", badAdjunction)
          , ("good data succeeds", goodScript)
          , ("bad data fails", badScript)
          ]
    ]

goodAdjunction :: Hedgehog.Property
goodAdjunction = Hedgehog.property $ do
  initialState <-
    Hedgehog.forAllWith ppChainState $
      pure $
        ChainState
          (initMock defaultBabbage (Value.singleton "" "" 1_000_000))
          mempty
          mempty
  datum <- Hedgehog.forAll $ runCtx (initialState.csMock) genAuctionDatum
  good <- Hedgehog.forAll $ runCtx (initialState.csMock) genGoodAuctionTest
  txTestGoodAdjunction auctionTest initialState datum good

badAdjunction :: Hedgehog.Property
badAdjunction = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith ppChainState _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBadAdjunction auctionTest initialState datum bad

goodScript :: Hedgehog.Property
goodScript = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith ppChainState _
  datum <- Hedgehog.forAll _
  good <- Hedgehog.forAll _
  txTestGood auctionTest initialState datum good

badScript :: Hedgehog.Property
badScript = Hedgehog.property $ do
  initialState <- Hedgehog.forAllWith ppChainState _
  datum <- Hedgehog.forAll _
  bad <- Hedgehog.forAll _
  txTestBad auctionTest initialState datum bad

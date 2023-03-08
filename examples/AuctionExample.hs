{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
[Notes]

\* Time is currently ignored - a scheme for handling it has not yet been
  developed

-}

module AuctionExample (AuctionTest, AuctionDatum, auctionTest) where

import GHC.Generics qualified as GHC

import Data.Maybe (fromJust, fromMaybe)

import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Contexts qualified as Plutus

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction, lower, raise))
import Hedgehog.Plutus.Diff (Diff' (Patch), diff, patch)
import Hedgehog.Plutus.ScriptContext (
  ScriptContext (
    ScriptContext,
    contextPurpose,
    contextRedeemer,
    contextTxInfo
  ),
  ScriptPurpose (Spending),
  ScriptType (Spend),
  plutusScriptContext,
 )
import Hedgehog.Plutus.TestData (
  EitherOr,
  Good',
  I (I),
  Mempty,
  ShouldBeNatural (MightBeNegative),
  ShouldEqual (MightNotEqual),
  Shouldn'tExist (MaybeExists, maybeExists),
  eitherOr,
  shouldBe,
  shouldBeSingletonList,
 )
import Hedgehog.Plutus.TestData.Plutus ()
import Hedgehog.Plutus.TxTest (
  ChainState (..),
  TxTest,
  omitted,
  txTest,
 )
import Plutus.Model qualified as Model
import Week01.EnglishAuction (
  Auction,
  AuctionAction (Close, MkBid),
  AuctionDatum (AuctionDatum, adAuction, adHighestBid),
  minLovelace,
 )
import Week01.Types (Auction (Auction, aCurrency, aMinBid, aSeller, aToken), Bid (Bid, bBid, bBidder))

data AuctionTest q = AuctionTest
  { stateRef :: !(q Plutus.TxOutRef)
  , otherInputsWithDatum ::
      !(q (ShouldEqual (Mempty [Plutus.TxInInfo]) [Plutus.TxInInfo]))
  , auctionRedeemer :: !(q (AuctionTestRedeemer I))
  }
  deriving stock (GHC.Generic)
deriving stock instance (Eq (AuctionTest I))
deriving stock instance (Eq (AuctionTest Good'))
deriving stock instance (Show (AuctionTest I))
deriving stock instance (Show (AuctionTest Good'))

data AuctionTestRedeemer q
  = TestRedeemerBid
      { testRedeemerBidder :: !(q Plutus.PubKeyHash)
      , testRedeemerBidMagnitude :: !(q (ShouldBeNatural Integer))
      -- ^ Difference between bid and minBid
      , selfOutputs :: !(q (EitherOr [Plutus.TxOut] (SelfOutput I)))
      , bidderOutputs ::
          !(q (EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Value)))
      }
  | TestRedeemerClose (q (CloseTest I))
  deriving stock (GHC.Generic)

deriving stock instance (Eq (AuctionTestRedeemer I))
deriving stock instance (Eq (AuctionTestRedeemer Good'))
deriving stock instance (Show (AuctionTestRedeemer I))
deriving stock instance (Show (AuctionTestRedeemer Good'))

data SelfOutput q = SelfOutput
  { selfDatum :: !(q (Shouldn'tExist (Patch AuctionDatum)))
  , selfValue :: !(q (Shouldn'tExist (Patch Plutus.Value)))
  }
  deriving stock (GHC.Generic)

deriving stock instance (Eq (SelfOutput I))
deriving stock instance (Eq (SelfOutput Good'))
deriving stock instance (Show (SelfOutput I))
deriving stock instance (Show (SelfOutput Good'))

{- | Invariant: If @datum.adHighestBid == Nothing@, then 'AuctionFailure', else
 'AuctionSuccess'
-}
data CloseTest q
  = AuctionFailure !(q (EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address)))
  | AuctionSuccess
      { bidderOutput ::
          !(q (EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address)))
      , sellerOutput ::
          !(q (EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address)))
      }
  deriving stock (GHC.Generic)

deriving stock instance (Eq (CloseTest I))
deriving stock instance (Eq (CloseTest Good'))
deriving stock instance (Show (CloseTest I))
deriving stock instance (Show (CloseTest Good'))

auctionTest :: TxTest ('Spend AuctionDatum) (AuctionTest I)
auctionTest = txTest $ \cs datum ->
  Adjunction
    { raise = raiseAuctionTest cs.csMock datum
    , lower = lowerAuctionTest cs.csMock datum
    }

raiseAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  ScriptContext AuctionAction ('Spend AuctionDatum) ->
  AuctionTest I
raiseAuctionTest
  Model.Mock {}
  inDatum@AuctionDatum {adHighestBid, adAuction = auction@Auction {aSeller}}
  sc@ScriptContext
    { contextRedeemer
    , contextTxInfo = txi@Plutus.TxInfo {txInfoInputs, txInfoOutputs}
    , contextPurpose = Spending ref
    } =
    AuctionTest
      { stateRef = I ref
      , otherInputsWithDatum =
          I $
            MightNotEqual
              ( filter
                  ((/= Plutus.NoOutputDatum) . (.txInInfoResolved.txOutDatum))
                  txInfoInputs
              )
      , auctionRedeemer =
          case contextRedeemer of
            MkBid bid@Bid {bBidder, bBid} ->
              I $
                TestRedeemerBid
                  { testRedeemerBidder = I bBidder
                  , testRedeemerBidMagnitude =
                      I $ MightBeNegative (bBid - minBid inDatum)
                  , selfOutputs =
                      I $
                        selfOutput bid
                          <$> shouldBeSingletonList
                            (Plutus.getContinuingOutputs psc)
                  , bidderOutputs =
                      I $
                        ( \o -> MaybeExists $ do
                            Bid {bBid} <- adHighestBid
                            (shouldBe (lovelaceValue bBid) o.txOutValue).maybeExists
                        )
                          <$> shouldBeSingletonList
                            ( filter
                                ( (== Plutus.pubKeyHashAddress bBidder)
                                    . (.txOutAddress)
                                )
                                txInfoOutputs
                            )
                  }
            Close ->
              I $
                TestRedeemerClose
                  ( maybe
                      ( I . AuctionFailure $
                          I $
                            shouldGetValue aSeller (token auction <> minValue)
                      )
                      ( \Bid {bBidder, bBid} ->
                          I $
                            AuctionSuccess
                              { bidderOutput =
                                  I $
                                    shouldGetValue
                                      bBidder
                                      (token auction <> minValue)
                              , sellerOutput =
                                  I $
                                    shouldGetValue
                                      aSeller
                                      (lovelaceValue bBid)
                              }
                      )
                      adHighestBid
                  )
      }
    where
      psc :: Plutus.ScriptContext
      psc = plutusScriptContext sc

      selfOutput ::
        Bid ->
        Plutus.TxOut ->
        SelfOutput I
      selfOutput rBid@Bid {bBid} o =
        SelfOutput
          { selfDatum =
              I . MaybeExists $
                diff
                  ( AuctionDatum
                      { adAuction = auction
                      , adHighestBid = Just rBid
                      }
                  )
                  (datum txi o)
          , selfValue =
              I . MaybeExists $
                diff
                  (token auction <> lovelaceValue (minLovelace + bBid))
                  o.txOutValue
          }

      shouldGetValue ::
        Plutus.PubKeyHash ->
        Plutus.Value ->
        EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address)
      shouldGetValue addr val =
        shouldBe (Plutus.pubKeyHashAddress addr) . (.txOutAddress)
          <$> shouldBeSingletonList
            (filter ((== val) . (.txOutValue)) txInfoOutputs)

lowerAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  AuctionTest I ->
  ScriptContext AuctionAction ('Spend AuctionDatum)
lowerAuctionTest
  Model.Mock {mockUtxos}
  inDatum@AuctionDatum {adAuction = auction@Auction {aSeller}, adHighestBid}
  (AuctionTest (I ref) (I (MightNotEqual otherIns)) rdmr) =
    ScriptContext
      { contextRedeemer = redeemer
      , contextPurpose = Spending ref
      , contextTxInfo =
          Plutus.TxInfo
            { txInfoInputs = otherIns
            , txInfoReferenceInputs = []
            , txInfoOutputs =
                case rdmr of
                  ( I
                      TestRedeemerBid
                        { testRedeemerBidder = I bidder
                        , testRedeemerBidMagnitude = I (MightBeNegative mag)
                        , selfOutputs = I so
                        }
                    ) -> continuingOutput bidder mag so
                  (I (TestRedeemerClose result)) ->
                    case (adHighestBid, result) of
                      (Nothing, I (AuctionFailure (I sellerOut))) ->
                        outputsForPkh
                          aSeller
                          (token auction <> lovelaceValue minLovelace)
                          sellerOut
                      ( Just Bid {bBidder, bBid}
                        , I
                            AuctionSuccess
                              { bidderOutput = (I bo)
                              , sellerOutput = (I so)
                              }
                        ) ->
                          outputsForPkh
                            bBidder
                            (token auction <> lovelaceValue minLovelace)
                            bo
                            <> outputsForPkh
                              aSeller
                              (lovelaceValue bBid)
                              so
                      (_, _) -> error "invariant violation"
            , txInfoFee = mempty
            , txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = Plutus.fromList []
            , txInfoValidRange = Plutus.always
            , txInfoSignatories = []
            , txInfoRedeemers = Plutus.fromList []
            , txInfoData = Plutus.fromList []
            , txInfoId = omitted
            }
      }
    where
      redeemer = case rdmr of
        ( I
            TestRedeemerBid
              { testRedeemerBidder = I bidder
              , testRedeemerBidMagnitude = I (MightBeNegative mag)
              }
          ) ->
            MkBid $
              Bid
                { bBidder = bidder
                , bBid = minBid inDatum + mag
                }
        (I TestRedeemerClose {}) -> Close

      continuingOutput ::
        Plutus.PubKeyHash ->
        Integer ->
        EitherOr [Plutus.TxOut] (SelfOutput I) ->
        [Plutus.TxOut]
      continuingOutput bidder mag =
        eitherOr
          ( \SelfOutput {selfDatum = I sd, selfValue = I sv} ->
              [ Plutus.TxOut
                  { txOutAddress = (mockUtxos Map.! ref).txOutAddress
                  , txOutValue =
                      patch
                        sv.maybeExists
                        ( token auction
                            <> lovelaceValue
                              (minLovelace + minBid inDatum + mag)
                        )
                  , txOutDatum =
                      Plutus.OutputDatum
                        . Plutus.Datum
                        . Plutus.toBuiltinData
                        $ patch
                          sd.maybeExists
                          ( AuctionDatum
                              { adAuction = auction
                              , adHighestBid =
                                  Just $
                                    Bid
                                      { bBidder = bidder
                                      , bBid = minBid inDatum + mag
                                      }
                              }
                          )
                  , txOutReferenceScript = Nothing
                  }
              ]
          )

      addrOutput :: Plutus.Address -> Plutus.Value -> Plutus.TxOut
      addrOutput addr val =
        Plutus.TxOut
          { txOutAddress = addr
          , txOutValue = val
          , txOutDatum = Plutus.NoOutputDatum
          , txOutReferenceScript = Nothing
          }

      outputsForPkh ::
        Plutus.PubKeyHash ->
        Plutus.Value ->
        EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address) ->
        [Plutus.TxOut]
      outputsForPkh pkh val =
        eitherOr
          ( \(MaybeExists mp) ->
              [addrOutput (fromMaybe (Plutus.pubKeyHashAddress pkh) mp) val]
          )

datum :: Plutus.TxInfo -> Plutus.TxOut -> AuctionDatum
datum i o = decodeDatum (resolveDatum i o)
  where
    decodeDatum :: Plutus.Datum -> AuctionDatum
    decodeDatum = fromJust . Plutus.fromBuiltinData . Plutus.getDatum

    resolveDatum :: Plutus.TxInfo -> Plutus.TxOut -> Plutus.Datum
    resolveDatum i o = case Plutus.txOutDatum o of
      Plutus.NoOutputDatum -> error "no datum"
      Plutus.OutputDatumHash dh -> fromJust $ Plutus.findDatum dh i
      Plutus.OutputDatum d -> d

token :: Auction -> Plutus.Value
token Auction {aCurrency, aToken} = Plutus.singleton aCurrency aToken 1

minBid :: AuctionDatum -> Integer
minBid AuctionDatum {adHighestBid, adAuction} =
  maybe adAuction.aMinBid ((+ 1) . (.bBid)) adHighestBid

lovelaceValue :: Integer -> Plutus.Value
lovelaceValue =
  Plutus.singleton
    (Plutus.CurrencySymbol "")
    (Plutus.TokenName "")

minValue :: Plutus.Value
minValue = lovelaceValue minLovelace

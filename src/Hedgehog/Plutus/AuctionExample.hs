{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
[Notes]

\* Time is currently ignored - a scheme for handling it has not yet been
  developed

-}

module Hedgehog.Plutus.AuctionExample (auctionTest) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC

import Data.Maybe (fromJust, fromMaybe)

import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Contexts qualified as Plutus
import PlutusTx qualified

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction, lower, raise))
import Hedgehog.Plutus.Diff (Diff' (Patch), diff, patch)
import Hedgehog.Plutus.Generics (Generically (Generically), Simple (Simple))
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
  Mempty,
  ShouldBeNatural (MightBeNegative),
  ShouldEqual (MightNotEqual),
  Shouldn'tExist (MaybeExists, maybeExists),
  TestData,
  eitherOr,
  shouldBe,
  shouldBeSingletonList,
 )
import Hedgehog.Plutus.TestData.Plutus ()
import Hedgehog.Plutus.TxTest (ChainState (ChainState, csMock), TxTest, omitted, txTest)

--- Copied from pioneer program

minLovelace :: Integer
minLovelace = 2000000

data Auction = Auction
  { aSeller :: !Plutus.PubKeyHash
  , aDeadline :: !Plutus.POSIXTime
  , aMinBid :: !Integer
  , aCurrency :: !Plutus.CurrencySymbol
  , aToken :: !Plutus.TokenName
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving (Diff') via (Generically Auction)

deriving via (Simple Plutus.PubKeyHash) instance Diff' Plutus.PubKeyHash
deriving via (Simple Plutus.CurrencySymbol) instance Diff' Plutus.CurrencySymbol
deriving via (Simple Plutus.TokenName) instance Diff' Plutus.TokenName
deriving via (Simple Plutus.POSIXTime) instance Diff' Plutus.POSIXTime
deriving via (Simple Plutus.Value) instance Diff' Plutus.Value

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bBidder :: !Plutus.PubKeyHash
  , bBid :: !Integer
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving (Diff') via (Generically Bid)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
  { adAuction :: !Auction
  , adHighestBid :: !(Maybe Bid)
  }
  deriving stock (Eq, Show, GHC.Generic)
  deriving (Diff') via (Generically AuctionDatum)

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

---

data AuctionTest = AuctionTest
  { stateRef :: !Plutus.TxOutRef
  , otherInputsWithDatum ::
      !(ShouldEqual (Mempty [Plutus.TxInInfo]) [Plutus.TxInInfo])
  , auctionRedeemer :: !AuctionTestRedeemer
  }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically AuctionTest)

type AuctionTestRedeemer :: Type
data AuctionTestRedeemer
  = TestRedeemerBid
      { testRedeemerBidder :: !Plutus.PubKeyHash
      , testRedeemerBidMagnitude :: !(ShouldBeNatural Integer)
      -- ^ Difference between bid and minBid
      , selfOutputs :: !(EitherOr [Plutus.TxOut] SelfOutput)
      , bidderOutputs ::
          !(EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Value))
      }
  | TestRedeemerClose CloseTest
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically AuctionTestRedeemer)

data SelfOutput = SelfOutput
  { selfDatum :: !(Shouldn'tExist (Patch AuctionDatum))
  , selfValue :: !(Shouldn'tExist (Patch Plutus.Value))
  }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically SelfOutput)

{- | Invariant: If @datum.adHighestBid == Nothing@, then 'AuctionFailure', else
 'AuctionSuccess'
-}
data CloseTest
  = AuctionFailure !(EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address))
  | AuctionSuccess
      { bidderOutput ::
          !(EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address))
      , sellerOutput ::
          !(EitherOr [Plutus.TxOut] (Shouldn'tExist Plutus.Address))
      }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically CloseTest)

auctionTest :: TxTest ('Spend AuctionDatum) AuctionTest
auctionTest = txTest $ \ChainState {csMock} datum ->
  Adjunction
    { raise = raiseAuctionTest csMock datum
    , lower = lowerAuctionTest csMock datum
    }

raiseAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  ScriptContext AuctionAction ('Spend AuctionDatum) ->
  AuctionTest
raiseAuctionTest
  Model.Mock {}
  inDatum@AuctionDatum {adHighestBid, adAuction = auction@Auction {aSeller}}
  sc@ScriptContext
    { contextRedeemer
    , contextTxInfo = txi@Plutus.TxInfo {txInfoInputs, txInfoOutputs}
    , contextPurpose = Spending ref
    } =
    AuctionTest
      { stateRef = ref
      , otherInputsWithDatum =
          MightNotEqual
            ( filter
                ((/= Plutus.NoOutputDatum) . (.txInInfoResolved.txOutDatum))
                txInfoInputs
            )
      , auctionRedeemer =
          case contextRedeemer of
            MkBid bid@Bid {bBidder, bBid} ->
              TestRedeemerBid
                { testRedeemerBidder = bBidder
                , testRedeemerBidMagnitude =
                    MightBeNegative (bBid - minBid inDatum)
                , selfOutputs =
                    selfOutput bid
                      <$> shouldBeSingletonList
                        (Plutus.getContinuingOutputs psc)
                , bidderOutputs =
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
              TestRedeemerClose
                ( maybe
                    ( AuctionFailure $
                        shouldGetValue aSeller (token auction <> minValue)
                    )
                    ( \Bid {bBidder, bBid} ->
                        AuctionSuccess
                          { bidderOutput =
                              shouldGetValue bBidder (token auction <> minValue)
                          , sellerOutput =
                              shouldGetValue aSeller (lovelaceValue bBid)
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
        SelfOutput
      selfOutput rBid@Bid {bBid} o =
        SelfOutput
          { selfDatum =
              MaybeExists $
                diff
                  ( AuctionDatum
                      { adAuction = auction
                      , adHighestBid = Just rBid
                      }
                  )
                  (datum txi o)
          , selfValue =
              MaybeExists $
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
  AuctionTest ->
  ScriptContext AuctionAction ('Spend AuctionDatum)
lowerAuctionTest
  Model.Mock {mockUtxos}
  inDatum@AuctionDatum {adAuction = auction@Auction {aSeller}, adHighestBid}
  (AuctionTest ref (MightNotEqual otherIns) rdmr) =
    ScriptContext
      { contextRedeemer = redeemer
      , contextPurpose = Spending ref
      , contextTxInfo =
          Plutus.TxInfo
            { txInfoInputs = otherIns
            , txInfoReferenceInputs = []
            , txInfoOutputs =
                case rdmr of
                  TestRedeemerBid
                    { testRedeemerBidder
                    , testRedeemerBidMagnitude = MightBeNegative mag
                    , selfOutputs
                    } ->
                      continuingOutput
                        testRedeemerBidder
                        mag
                        selfOutputs
                  TestRedeemerClose result ->
                    case (adHighestBid, result) of
                      (Nothing, AuctionFailure sellerOut) ->
                        outputsForPkh
                          aSeller
                          (token auction <> lovelaceValue minLovelace)
                          sellerOut
                      ( Just Bid {bBidder, bBid}
                        , AuctionSuccess {bidderOutput, sellerOutput}
                        ) ->
                          outputsForPkh
                            bBidder
                            (token auction <> lovelaceValue minLovelace)
                            bidderOutput
                            <> outputsForPkh
                              aSeller
                              (lovelaceValue bBid)
                              sellerOutput
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
        TestRedeemerBid
          { testRedeemerBidder
          , testRedeemerBidMagnitude = MightBeNegative mag
          } ->
            MkBid $
              Bid
                { bBidder = testRedeemerBidder
                , bBid = minBid inDatum + mag
                }
        TestRedeemerClose {} -> Close

      continuingOutput ::
        Plutus.PubKeyHash ->
        Integer ->
        EitherOr [Plutus.TxOut] SelfOutput ->
        [Plutus.TxOut]
      continuingOutput bidder mag =
        eitherOr
          ( \SelfOutput {selfDatum, selfValue} ->
              [ Plutus.TxOut
                  { txOutAddress = (mockUtxos Map.! ref).txOutAddress
                  , txOutValue =
                      patch
                        selfValue.maybeExists
                        ( token auction
                            <> lovelaceValue
                              (minLovelace + minBid inDatum + mag)
                        )
                  , txOutDatum =
                      Plutus.OutputDatum
                        . Plutus.Datum
                        . Plutus.toBuiltinData
                        $ patch
                          selfDatum.maybeExists
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

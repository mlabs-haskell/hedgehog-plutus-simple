{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-
[Notes]

\* Time is currently ignored - a scheme for handling it has not yet been
  developed

-}

module Hedgehog.Plutus.AuctionExample (auctionTest) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC

import Prelude hiding ((.))

import Control.Category ((.))
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Contexts qualified as Plutus
import PlutusTx qualified

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Diff
import Hedgehog.Plutus.Generics
import Hedgehog.Plutus.ScriptContext
import Hedgehog.Plutus.TestData
import Hedgehog.Plutus.TxTest

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

deriving via (Simple Plutus.TxOutRef) instance TestData Plutus.TxOutRef
deriving via (Simple Plutus.PubKeyHash) instance TestData Plutus.PubKeyHash

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
      , selfOutputs ::
          !( Either
              [Plutus.TxOut]
              (Maybe (Patch SelfOutput))
           )
      , bidderOutputs :: !(Either [Plutus.TxOut] BidderOutput)
      }
  | TestRedeemerClose
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically AuctionTestRedeemer)

data SelfOutput = SelfOutput
  { selfDatum :: !AuctionDatum
  , selfValue :: !Plutus.Value
  }
  deriving stock (Eq, GHC.Generic)
  deriving (Diff') via (Generically SelfOutput)

data BidderOutput
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically BidderOutput)

auctionTest :: TxTest ('Spend AuctionDatum) AuctionTest
auctionTest = txTest $ \mock datum ->
  Adjunction
    { raise = raiseAuctionTest mock datum
    , lower = lowerAuctionTest mock datum
    }

raiseAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  ScriptContext AuctionAction ('Spend AuctionDatum) ->
  AuctionTest
raiseAuctionTest
  Model.Mock {}
  inDatum
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
                ( \Plutus.TxInInfo {Plutus.txInInfoResolved} ->
                    Plutus.txOutDatum txInInfoResolved
                      /= Plutus.NoOutputDatum
                )
                txInfoInputs
            )
      , auctionRedeemer =
          case contextRedeemer of
            MkBid bid@Bid {bBidder, bBid} ->
              TestRedeemerBid
                { testRedeemerBidder = bBidder
                , testRedeemerBidMagnitude =
                    MightBeNegative
                      ( bBid
                          - minBid
                            txi
                            ( Plutus.txInInfoResolved
                                . fromJust
                                $ Plutus.findOwnInput psc
                            )
                      )
                , selfOutputs =
                    selfOutput bid
                      <$> shouldBeSingletonList
                        (Plutus.getContinuingOutputs psc)
                , bidderOutputs =
                    _
                      <$> shouldBeSingletonList
                        ( filter
                            ( \o ->
                                o.txOutAddress
                                  == Plutus.pubKeyHashAddress bBidder
                            )
                            txInfoOutputs
                        )
                }
            Close -> TestRedeemerClose
      }
    where
      psc :: Plutus.ScriptContext
      psc = plutusScriptContext sc

      selfOutput ::
        Bid ->
        Plutus.TxOut ->
        Maybe (Patch SelfOutput)
      selfOutput rBid o =
        diff
          ( SelfOutput
              { selfDatum =
                  AuctionDatum
                    { adAuction = inDatum.adAuction
                    , adHighestBid = Just rBid
                    }
              , selfValue =
                  token inDatum.adAuction
                    <> lovelaceValue (minLovelace + rBid.bBid)
              }
          )
          ( SelfOutput
              { selfDatum = datum txi o
              , selfValue = o.txOutValue
              }
          )

lowerAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  AuctionTest ->
  ScriptContext AuctionAction ('Spend AuctionDatum)
lowerAuctionTest
  Model.Mock {mockUtxos}
  AuctionDatum {adAuction}
  (AuctionTest ref (MightNotEqual otherIns) rdmr) =
    ScriptContext
      { contextRedeemer = redeemer
      , contextPurpose = Spending ref
      , contextTxInfo =
          Plutus.TxInfo
            { txInfoInputs = otherIns
            , txInfoReferenceInputs = []
            , txInfoOutputs = continuingOutput
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
        TestRedeemerBid bidder (MightBeNegative mag) _ _ ->
          MkBid $
            Bid
              { bBidder = bidder
              , bBid = adAuction.aMinBid + mag
              }
        TestRedeemerClose -> Close

      continuingOutput :: [Plutus.TxOut]
      continuingOutput =
        case rdmr of
          TestRedeemerBid bidder (MightBeNegative mag) selfOutputs _ ->
            eitherOr
              ( \p ->
                  [ selfOutput $
                      patch
                        p
                        SelfOutput
                          { selfDatum =
                              AuctionDatum
                                { adAuction = adAuction
                                , adHighestBid =
                                    Just $
                                      Bid
                                        { bBidder = bidder
                                        , bBid = adAuction.aMinBid + mag
                                        }
                                }
                          , selfValue =
                              token adAuction
                                <> lovelaceValue
                                  ( minLovelace
                                      + adAuction.aMinBid
                                      + mag
                                  )
                          }
                  ]
              )
              selfOutputs
          TestRedeemerClose -> []

      selfOutput :: SelfOutput -> Plutus.TxOut
      selfOutput (SelfOutput dat val) =
        Plutus.TxOut
          { txOutAddress = (mockUtxos Map.! ref).txOutAddress
          , txOutValue = val
          , txOutDatum =
              Plutus.OutputDatum
                . Plutus.Datum
                . Plutus.toBuiltinData
                $ dat
          , txOutReferenceScript = Nothing
          }

decodeDatum :: Plutus.Datum -> AuctionDatum
decodeDatum = fromJust . Plutus.fromBuiltinData . Plutus.getDatum

datum :: Plutus.TxInfo -> Plutus.TxOut -> AuctionDatum
datum i o = decodeDatum (resolveDatum i o)

auction :: Plutus.TxInfo -> Plutus.TxOut -> Auction
auction o = (.adAuction) . datum o

resolveDatum :: Plutus.TxInfo -> Plutus.TxOut -> Plutus.Datum
resolveDatum i o = case Plutus.txOutDatum o of
  Plutus.NoOutputDatum -> error "no datum"
  Plutus.OutputDatumHash dh -> fromJust $ Plutus.findDatum dh i
  Plutus.OutputDatum d -> d

token :: Auction -> Plutus.Value
token Auction {aCurrency, aToken} = Plutus.singleton aCurrency aToken 1

hiBid :: Plutus.TxInfo -> Plutus.TxOut -> Maybe Bid
hiBid o = (.adHighestBid) . datum o

minBid :: Plutus.TxInfo -> Plutus.TxOut -> Integer
minBid i o =
  maybe (auction i o).aMinBid ((+ 1) . (.bBid)) (hiBid i o)

lovelaceValue :: Integer -> Plutus.Value
lovelaceValue =
  Plutus.singleton
    (Plutus.CurrencySymbol "")
    (Plutus.TokenName "")

unsafeGetDatum ::
  Map Plutus.DatumHash Plutus.Datum ->
  Plutus.OutputDatum ->
  Plutus.Datum
unsafeGetDatum datums = \case
  Plutus.OutputDatum d -> d
  Plutus.OutputDatumHash dh -> datums Map.! dh
  Plutus.NoOutputDatum -> error "No datum!"
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-
[Notes]

\* Time is currently ignored - a scheme for handling it has not yet been
  developed

-}

module Hedgehog.Plutus.AuctionExample where

import Data.Kind (Type)
import GHC.Generics qualified as GHC

import Prelude hiding ((.))

import Control.Category ((.))
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Contexts qualified as Plutus
import PlutusTx qualified

import Plutus.Model qualified as Model

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Diff
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

-- data AuctionTest = AuctionTest
--   { stateRef :: Plutus.TxOutRef
--   , action :: AuctionAct
--   }

-- data AuctionAct
--   = BidAct
--       { bidAmount :: Integer
--       , bidder :: Plutus.PubKeyHash
--       }
--   | CloseAct

-- data instance Bad AuctionTest = BadAuctionTest
--   { otherInputswithDatum :: !(Set TxIn)
--   , hasGoodDatum :: !(Either Plutus.Datum HasGoodDatum)
--   }

-- data HasGoodDatum = HasGoodDatum
--   {incorrectInputValue :: Maybe Plutus.Value}

-- auctionTest :: TxContext -> Plutus.TxOutRef -> TxTest AuctionTest
-- auctionTest txc ref = txTest _ (goodAuction txc ref) (raiseAuction txc)

-- goodAuction :: TxContext -> Plutus.TxOutRef -> AuctionTest -> ScriptTx
-- goodAuction
--   TxContext {mockchain = Model.Mock {Model.mockUtxos}, datums}
--   ref
--   (AuctionTest sr act) =
--     ScriptTx
--       { scriptTxPurpose =
--           Spending
--             ref
--             ( InScript
--                 { inScriptSource = InTransaction
--                 , inScriptRedeemer =
--                     Just
--                       . Plutus.Redeemer
--                       . Plutus.toBuiltinData
--                       $ redeemer
--                 }
--             )
--       , scriptTx =
--           Tx
--             { txInputs = Set.empty
--             , txOutputs = outs
--             , txMint = Map.empty
--             , txFee = 0
--             , txValidRange = Plutus.always
--             , txExtraSignatures = Set.empty
--             }
--       }
--     where
--       inUTxO :: Plutus.TxOut
--       inUTxO = mockUtxos Map.! sr

--       inDatum :: AuctionDatum
--       inDatum =
--         Plutus.unsafeFromBuiltinData . Plutus.getDatum $
--           case Plutus.txOutDatum inUTxO of
--             Plutus.NoOutputDatum -> error "No datum!"
--             Plutus.OutputDatum d -> d
--             Plutus.OutputDatumHash dh -> datums Map.! dh

--       sh :: Plutus.ScriptHash
--       sh = case Plutus.addressCredential . Plutus.txOutAddress $ inUTxO of
--         Plutus.PubKeyCredential _ -> error "something is badly wrong!"
--         Plutus.ScriptCredential sh' -> sh'

--       auction :: Auction
--       auction = adAuction inDatum

--       token :: Plutus.Value
--       token = Plutus.singleton (aCurrency auction) (aToken auction) 1

--       outs :: Vector TxOut
--       outs =
--         case act of
--           BidAct {bidAmount, bidder} ->
--             [ TxOut
--                 { txOutAddress = Plutus.scriptHashAddress sh
--                 , txOutValue =
--                     token <> lovelaceValue (minLovelace + bidAmount)
--                 , txOutDatum =
--                     Just
--                       . Plutus.Datum
--                       . Plutus.toBuiltinData
--                       $ inDatum
--                         { adHighestBid =
--                             Just
--                               Bid
--                                 { bBid = bidAmount
--                                 , bBidder = bidder
--                                 }
--                         }
--                 , txOutReferenceScript = Nothing
--                 }
--             ]
--               <> maybe
--                 []
--                 ( \Bid {bBid, bBidder} ->
--                     [ TxOut
--                         { txOutAddress = Plutus.pubKeyHashAddress bBidder
--                         , txOutValue = lovelaceValue bBid
--                         , txOutDatum = Nothing
--                         , txOutReferenceScript = Nothing
--                         }
--                     ]
--                 )
--                 (adHighestBid inDatum)
--           CloseAct ->
--             maybe
--               [ TxOut
--                   { txOutAddress = Plutus.pubKeyHashAddress (aSeller auction)
--                   , txOutValue = token <> minValue
--                   , txOutDatum = Nothing
--                   , txOutReferenceScript = Nothing
--                   }
--               ]
--               ( \Bid {bBid, bBidder} ->
--                   [ TxOut
--                       { txOutAddress =
--                           Plutus.pubKeyHashAddress (aSeller auction)
--                       , txOutValue =
--                           lovelaceValue bBid <> minValue
--                       , txOutDatum = Nothing
--                       , txOutReferenceScript = Nothing
--                       }
--                   , TxOut
--                       { txOutAddress = Plutus.pubKeyHashAddress bBidder
--                       , txOutValue = token <> minValue
--                       , txOutDatum = Nothing
--                       , txOutReferenceScript = Nothing
--                       }
--                   ]
--               )
--               (adHighestBid inDatum)

--       redeemer :: AuctionAction
--       redeemer = case act of
--         BidAct {bidAmount, bidder} ->
--           MkBid $ Bid {bBid = bidAmount, bBidder = bidder}
--         CloseAct -> Close

-- raiseAuction :: TxContext -> ScriptTx -> Either (Bad AuctionTest) AuctionTest
-- raiseAuction
--   TxContext {mockchain = Model.Mock {Model.mockUtxos}, datums}
--   ScriptTx
--     { scriptTx
--     , scriptTxPurpose = Spending inRef InScript {}
--     } =
--     if isGood
--       then Right AuctionTest {}
--       else Left mkBad
--     where
--       mkBad :: Bad AuctionTest
--       mkBad =
--         BadAuctionTest
--           { otherInputswithDatum = otherInputswithDatum'
--           , hasGoodDatum =
--               auctionDatum <&> \ad ->
--                 HasGoodDatum
--                   { incorrectInputValue = incorrectInputValue' ad
--                   }
--           }

--       isGood :: Bool
--       isGood =
--         and @[]
--           [ Set.null otherInputswithDatum'
--           , isNothing (fromRight auctionDatum >>= incorrectInputValue')
--           ]

--       otherInputswithDatum' :: Set TxIn
--       otherInputswithDatum' =
--         Set.filter
--           ( \inp ->
--               Plutus.txOutDatum (mockUtxos Map.! txInRef inp)
--                 /= Plutus.NoOutputDatum
--           )
--           . txInputs
--           $ scriptTx

--       inUTxO :: Plutus.TxOut
--       inUTxO = mockUtxos Map.! inRef

--       inVal :: Plutus.Value
--       inVal = Plutus.txOutValue inUTxO

--       datum :: Plutus.Datum
--       datum = case Plutus.txOutDatum inUTxO of
--         Plutus.NoOutputDatum -> error "No datum present"
--         Plutus.OutputDatumHash dh -> datums Map.! dh
--         Plutus.OutputDatum d -> d

--       auctionDatum :: Either Plutus.Datum AuctionDatum
--       auctionDatum =
--         maybe
--           (Left datum)
--           Right
--           (Plutus.fromBuiltinData . Plutus.getDatum $ datum)

--       token :: AuctionDatum -> Plutus.Value
--       token AuctionDatum {adAuction = Auction {aCurrency, aToken}} =
--         Plutus.singleton aCurrency aToken 1

--       incorrectInputValue' :: AuctionDatum -> Maybe Plutus.Value
--       incorrectInputValue' ad =
--         if inVal
--           == token ad
--             <> minValue
--             <> foldMap (lovelaceValue . bBid) (adHighestBid ad)
--           then Nothing
--           else Just inVal
-- raiseAuction _ _ = error "Not a spending input"

-- minValue :: Plutus.Value
-- minValue = lovelaceValue minLovelace

-- lovelaceValue :: Integer -> Plutus.Value
-- lovelaceValue =
--   Plutus.singleton
--     (Plutus.CurrencySymbol "")
--     (Plutus.TokenName "")

-- fromRight :: Either a b -> Maybe b
-- fromRight (Left _) = Nothing
-- fromRight (Right r) = Just r

data AuctionTest q = AuctionTest
  { stateRef :: !(Only Plutus.TxOutRef q)
  , otherInputsWithDatum :: !(ShouldEqual (Mempty [Plutus.TxInInfo]) q)
  , auctionRedeemer :: !(AuctionTestRedeemer q)
  }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically' AuctionTest)

type AuctionTestRedeemer :: Quality -> Type
data AuctionTestRedeemer q
  = TestRedeemerBid
      { testRedeemerBidder :: !(Only Plutus.PubKeyHash q)
      , testRedeemerBidMagnitude :: !(ShouldBeNatural q)
      -- ^ Difference between bid and minBid
      , selfOutputs :: !(EitherOr (Only [Plutus.TxOut]) SelfOutput q)
      }
  | TestRedeemerClose
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically' AuctionTestRedeemer)

data SelfOutput q = SelfOutput
  { datumIfDifferent :: Shouldn'tBePresent (Only (Patch (Generically AuctionDatum))) q
  , valueIfDifferent :: Shouldn'tBePresent (Only Plutus.Value) q
  }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically' SelfOutput)

auctionTest :: TxTest ('Spend AuctionDatum) AuctionTest
auctionTest = txTest $ \mock datum ->
  Adjunction
    { raise = raiseAuctionTest mock datum
    , lower = lowerAuctionTest mock
    }

raiseAuctionTest ::
  Model.Mock ->
  AuctionDatum ->
  ScriptContext AuctionAction ('Spend AuctionDatum) ->
  Generalised AuctionTest
raiseAuctionTest
  Model.Mock {}
  inDatum
  sc@ScriptContext
    { contextRedeemer
    , contextTxInfo = txi@Plutus.TxInfo {Plutus.txInfoInputs}
    , contextPurpose = Spending ref
    } =
    AuctionTest
      { stateRef = Only ref
      , otherInputsWithDatum =
          MightNotBeEqual
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
                { testRedeemerBidder = Only bBidder
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
                    case Plutus.getContinuingOutputs psc of
                      [o] -> ShouldBe (selfOutput o bid)
                      os -> Shouldn'tBe (Only os)
                }
            Close -> TestRedeemerClose
      }
    where
      psc :: Plutus.ScriptContext
      psc = plutusScriptContext sc

      selfOutput :: Plutus.TxOut -> Bid -> SelfOutput 'IsGeneralised
      selfOutput o rBid =
        SelfOutput
          ( expect
              ( AuctionDatum
                  { adAuction = inDatum.adAuction
                  , adHighestBid = Just rBid
                  }
              )
              outDatum
          )
          ( expect
              ( token
                  inDatum.adAuction
                  <> lovelaceValue (minLovelace + rBid.bBid)
              )
              (Plutus.txOutValue o)
          )
        where
          outDatum :: AuctionDatum
          outDatum = datum txi o

lowerAuctionTest ::
  Model.Mock ->
  Generalised AuctionTest ->
  ScriptContext AuctionAction ('Spend AuctionDatum)
lowerAuctionTest = undefined

--   txc@TxContext {mockchain = Model.Mock {Model.mockUtxos}}
--   (AuctionTest (Only ref) (MightNotBeEqual otherIns) rdmr) =
--     ScriptTx
--       ( Tx
--           { txInputs = otherIns
--           , txOutputs = case rdmr of
--               TestRedeemerBid
--                 (Only bidder)
--                 (MightBeNegative mag)
--                 selfOutputs -> case selfOutputs of
--                   Shouldn'tBe (Only os) -> os
--                   ShouldBe (SelfOutput dat val) ->
--                     Vector.singleton (continuingOut mag bidder dat val)
--               TestRedeemerClose -> Vector.empty
--           , txMint = Map.empty
--           , txFee = 0
--           , txValidRange = Plutus.always
--           , txExtraSignatures = Set.empty
--           }
--       )
--       ( Spending
--           ref
--           ( InScript
--               InTransaction
--               ( Just
--                   ( Plutus.Redeemer . Plutus.toBuiltinData $
--                       case rdmr of
--                         TestRedeemerBid (Only bidder) (MightBeNegative mag) _ ->
--                           MkBid $ bid txc bidder ref mag
--                         TestRedeemerClose -> Close
--                   , _
--                   )
--               )
--           )
--       )
--     where
--       bid :: TxContext -> Plutus.PubKeyHash -> Plutus.TxOutRef -> Integer -> Bid
--       bid txc bidder ref mag =
--         Bid
--           { bBidder = bidder
--           , bBid = bidAmt txc ref mag
--           }

--       continuingOut ::
--         Integer ->
--         Plutus.PubKeyHash ->
--         Generalised (Shouldn'tBePresent AuctionTestDatum) ->
--         Generalised (Shouldn'tBePresent (Only Plutus.Value)) ->
--         TxOut
--       continuingOut mag bidder dat val =
--         TxOut
--           { txOutAddress = selfAddress mockUtxos ref
--           , txOutValue = case val of
--               IsPresent (Only v) -> v
--               IsAbsent ->
--                 token (auction txc ref)
--                   <> lovelaceValue (minLovelace + bidAmt txc ref mag)
--           , txOutDatum =
--               Just . Plutus.Datum . Plutus.toBuiltinData $
--                 AuctionDatum
--                   { adAuction = case dat of
--                       IsPresent
--                         AuctionTestDatum
--                           { auctionTestAuction = IsPresent (Only auct)
--                           } -> auct
--                       _ -> auction txc ref
--                   , adHighestBid = case dat of
--                       IsPresent
--                         AuctionTestDatum
--                           { auctionTestHiBid = IsPresent (Only hiBid)
--                           } -> hiBid
--                       _ -> Just $ bid txc bidder ref mag
--                   }
--           , txOutReferenceScript = Nothing
--           }

-- bidAmt :: TxContext -> Plutus.TxOutRef -> Integer -> Integer
-- bidAmt txc ref mag = minBid txc ref + mag

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

selfAddress ::
  Map Plutus.TxOutRef Plutus.TxOut ->
  Plutus.TxOutRef ->
  Plutus.Address
selfAddress utxos ref = Plutus.txOutAddress (utxos Map.! ref)

minValue :: Plutus.Value
minValue = lovelaceValue minLovelace

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

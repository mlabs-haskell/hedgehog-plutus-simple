{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx qualified

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.TestData
import Hedgehog.Plutus.TestSingleScript
import Hedgehog.Plutus.Tx

import Data.Vector qualified as Vector
import Plutus.Model qualified as Model

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
  deriving stock (Eq, Show)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bBidder :: !Plutus.PubKeyHash
  , bBid :: !Integer
  }
  deriving stock (Eq, Show)

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
  deriving stock (Show)

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
  , otherInputsWithDatum :: !(ShouldEqual (Mempty (Set TxIn)) q)
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
      , selfOutputs :: !(EitherOr (Only (Vector TxOut)) SelfOutput q)
      }
  | TestRedeemerClose
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically' AuctionTestRedeemer)

data SelfOutput q = SelfOutput
  { datumIfDifferent :: Shouldn'tBePresent AuctionTestDatum q
  , valueIfDifferent :: Shouldn'tBePresent (Only Plutus.Value) q
  }
  deriving stock (GHC.Generic)
  deriving (TestData) via (Generically' SelfOutput)

data AuctionTestDatum q = AuctionTestDatum
  { auctionTestAuction :: !(Shouldn'tBePresent (Only Auction) q)
  , auctionTestHiBid :: !(Shouldn'tBePresent (Only (Maybe Bid)) q)
  }

auctionTest :: TxTest AuctionTest
auctionTest = txTest $ \txc ->
  Adjunction
    { raise = raiseAuctionTest txc
    , lower = lowerAuctionTest txc
    }

raiseAuctionTest :: TxContext -> ScriptTx -> AuctionTest 'IsGeneralised
raiseAuctionTest
  txc@TxContext {mockchain = Model.Mock {Model.mockUtxos}}
  ScriptTx
    { scriptTx = Tx {txInputs, txOutputs}
    , scriptTxPurpose = Spending ref (InScript _ (Just (rdmr, _)))
    } =
    AuctionTest
      { stateRef = Only ref
      , otherInputsWithDatum =
          MightNotBeEqual
            ( Set.filter
                ( \inp' ->
                    Plutus.txOutDatum (mockUtxos Map.! txInRef inp')
                      /= Plutus.NoOutputDatum
                )
                txInputs
            )
      , auctionRedeemer =
          case fromJust
            . Plutus.fromBuiltinData
            $ Plutus.getRedeemer rdmr of
            MkBid bid@Bid {bBidder, bBid} ->
              TestRedeemerBid
                { testRedeemerBidder = Only bBidder
                , testRedeemerBidMagnitude =
                    MightBeNegative (bBid - minBid txc ref)
                , selfOutputs =
                    let os =
                          Vector.filter
                            ( \TxOut {txOutAddress} ->
                                txOutAddress == selfAddress mockUtxos ref
                            )
                            txOutputs
                     in if Vector.length os == 1
                          then ShouldBe (selfOutput (Vector.head os) bid)
                          else Shouldn'tBe (Only os)
                }
            Close -> TestRedeemerClose
      }
    where
      selfOutput :: TxOut -> Bid -> SelfOutput 'IsGeneralised
      selfOutput o rBid =
        SelfOutput
          ( if correctAuction && correctBid
              then IsAbsent
              else
                IsPresent
                  ( AuctionTestDatum
                      { auctionTestAuction =
                          expect (adAuction inDatum) (adAuction outDatum)
                      , auctionTestHiBid =
                          expect (Just rBid) (adHighestBid outDatum)
                      }
                  )
          )
          ( expect
              ( token
                  (adAuction inDatum)
                  <> lovelaceValue (minLovelace + bBid rBid)
              )
              (txOutValue o)
          )
        where
          outDatum :: AuctionDatum
          outDatum =
            fromJust
              . Plutus.fromBuiltinData
              . Plutus.getDatum
              . fromJust
              . txOutDatum
              $ o

          inDatum :: AuctionDatum
          inDatum = datum txc ref

          correctAuction :: Bool
          correctAuction = adAuction outDatum == adAuction inDatum

          correctBid :: Bool
          correctBid = adHighestBid outDatum == Just rBid
raiseAuctionTest _ _ = error "not a spending input, or no redeemer"

lowerAuctionTest :: TxContext -> AuctionTest 'IsGeneralised -> ScriptTx
lowerAuctionTest
  txc@TxContext {mockchain = Model.Mock {Model.mockUtxos}}
  (AuctionTest (Only ref) (MightNotBeEqual otherIns) rdmr) =
    ScriptTx
      ( Tx
          { txInputs = otherIns
          , txOutputs = case rdmr of
              TestRedeemerBid
                (Only bidder)
                (MightBeNegative mag)
                selfOutputs -> case selfOutputs of
                  Shouldn'tBe (Only os) -> os
                  ShouldBe (SelfOutput dat val) ->
                    Vector.singleton (continuingOut mag bidder dat val)
              TestRedeemerClose -> Vector.empty
          , txMint = Map.empty
          , txFee = 0
          , txValidRange = Plutus.always
          , txExtraSignatures = Set.empty
          }
      )
      ( Spending
          ref
          ( InScript
              InTransaction
              ( Just
                  ( Plutus.Redeemer . Plutus.toBuiltinData $
                      case rdmr of
                        TestRedeemerBid (Only bidder) (MightBeNegative mag) _ ->
                          MkBid $ bid txc bidder ref mag
                        TestRedeemerClose -> Close
                  , _
                  )
              )
          )
      )
    where
      bid :: TxContext -> Plutus.PubKeyHash -> Plutus.TxOutRef -> Integer -> Bid
      bid txc bidder ref mag =
        Bid
          { bBidder = bidder
          , bBid = bidAmt txc ref mag
          }

      continuingOut ::
        Integer ->
        Plutus.PubKeyHash ->
        Generalised (Shouldn'tBePresent AuctionTestDatum) ->
        Generalised (Shouldn'tBePresent (Only Plutus.Value)) ->
        TxOut
      continuingOut mag bidder dat val =
        TxOut
          { txOutAddress = selfAddress mockUtxos ref
          , txOutValue = case val of
              IsPresent (Only v) -> v
              IsAbsent ->
                token (auction txc ref)
                  <> lovelaceValue (minLovelace + bidAmt txc ref mag)
          , txOutDatum =
              Just . Plutus.Datum . Plutus.toBuiltinData $
                AuctionDatum
                  { adAuction = case dat of
                      IsPresent
                        AuctionTestDatum
                          { auctionTestAuction = IsPresent (Only auct)
                          } -> auct
                      _ -> auction txc ref
                  , adHighestBid = case dat of
                      IsPresent
                        AuctionTestDatum
                          { auctionTestHiBid = IsPresent (Only hiBid)
                          } -> hiBid
                      _ -> Just $ bid txc bidder ref mag
                  }
          , txOutReferenceScript = Nothing
          }

bidAmt :: TxContext -> Plutus.TxOutRef -> Integer -> Integer
bidAmt txc ref mag = minBid txc ref + mag

decodeDatum :: Plutus.Datum -> AuctionDatum
decodeDatum = fromJust . Plutus.fromBuiltinData . Plutus.getDatum

datum :: TxContext -> Plutus.TxOutRef -> AuctionDatum
datum TxContext {mockchain = Model.Mock {Model.mockUtxos}, datums} ref =
  decodeDatum . unsafeGetDatum datums . Plutus.txOutDatum $ mockUtxos Map.! ref

auction :: TxContext -> Plutus.TxOutRef -> Auction
auction txc = adAuction . datum txc

token :: Auction -> Plutus.Value
token Auction {aCurrency, aToken} = Plutus.singleton aCurrency aToken 1

hiBid :: TxContext -> Plutus.TxOutRef -> Maybe Bid
hiBid txc = adHighestBid . datum txc

minBid :: TxContext -> Plutus.TxOutRef -> Integer
minBid txc ref =
  maybe (aMinBid $ auction txc ref) ((+ 1) . bBid) (hiBid txc ref)

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

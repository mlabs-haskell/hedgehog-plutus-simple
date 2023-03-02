{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoOverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-deriving-strategies -Wno-incomplete-uni-patterns #-}

-- Disable warnings to allow minimizing change from the original

module Week01.EnglishAuction (
  AuctionDatum (..),
  AuctionAction (..),
  Auction,
  minLovelace,
  mkAuctionValidator,
  typedAuctionValidator,
) where

import GHC.Generics (Generic)

import Cardano.Simple.PlutusLedgerApi.V1.Scripts (ValidatorHash (..))
import Plutus.Model.V1 (Validator, txOutDatumHash)
import Plutus.Model.V2 (TypedValidator, Versioned (..), unTypedValidator, validatorHash)
import Plutus.Model.Validator.V1 (mkTypedValidator)
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (contains, from)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Address,
  CurrencySymbol,
  Datum (Datum),
  POSIXTime,
  PubKeyHash,
  ScriptContext,
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoValidRange),
  TxOut (txOutAddress),
  Value,
  adaSymbol,
  adaToken,
  to,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (
    scriptContextTxInfo
  ),
  TxInfo (txInfoOutputs),
  findDatum,
  getContinuingOutputs,
 )
import PlutusLedgerApi.V2.Tx (TxOut (txOutValue))
import PlutusTx qualified
import PlutusTx.Prelude (
  AdditiveSemigroup ((+)),
  Bool (..),
  BuiltinData,
  Eq ((==)),
  Integer,
  Maybe (Just, Nothing),
  Ord ((>=)),
  Semigroup ((<>)),
  const,
  error,
  fromMaybe,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
 )
import Week01.Types (
  Auction (aCurrency, aDeadline, aMinBid, aSeller, aToken),
  AuctionAction (..),
  AuctionDatum (..),
  Bid (..),
 )

lovelaceValueOf :: Integer -> Value
lovelaceValueOf = Value.singleton adaSymbol adaToken

-- can't build plutus-ledger to import this

minLovelace :: Integer
minLovelace = 2000000

-- data Auctioning where
-- instance ValidatorTypes Auctioning where
--    type instance RedeemerType Auctioning = AuctionAction
--    type instance DatumType Auctioning = AuctionDatum

{-# INLINEABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum {..} = case adHighestBid of
  Nothing -> aMinBid adAuction
  Just Bid {..} -> bBid + 1

{-# INLINEABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
  traceIfFalse "wrong input value" correctInputValue
    && case redeemer of
      MkBid b@Bid {..} ->
        traceIfFalse "bid too low" (sufficientBid bBid)
          && traceIfFalse "wrong output datum" (correctBidOutputDatum b)
          && traceIfFalse "wrong output value" (correctBidOutputValue bBid)
          && traceIfFalse "wrong refund" correctBidRefund
          && traceIfFalse "too late" correctBidSlotRange
      Close ->
        traceIfFalse "too early" correctCloseSlotRange
          && case adHighestBid ad of
            Nothing ->
              traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> lovelaceValueOf minLovelace)
            Just Bid {..} ->
              traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> lovelaceValueOf minLovelace)
                && traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ lovelaceValueOf bBid)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _ -> True
          xs = [i | i <- txInfoInputs info, isScriptInput i]
       in case xs of
            [i] -> i
            _ -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue =
      inVal == case adHighestBid ad of
        Nothing -> tokenValue <> lovelaceValueOf minLovelace
        Just Bid {..} -> tokenValue <> lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just ad' -> (o, ad')
            Nothing -> traceError "error decoding data"
      _ -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b =
      (adAuction outputDatum == auction)
        && (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
      txOutValue ownOutput == tokenValue <> lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
      Nothing -> True
      Just Bid {..} ->
        let os =
              [ o
              | o <- txInfoOutputs info
              , txOutAddress o == pubKeyHashAddress bBidder -- Nothing
              ]
         in case os of
              [o] -> txOutValue o == lovelaceValueOf bBid
              _ -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v =
      let [o] =
            [ o'
            | o' <- txInfoOutputs info
            , txOutValue o' == v
            ]
       in txOutAddress o == pubKeyHashAddress h -- Nothing

typedAuctionValidator :: TypedValidator AuctionDatum AuctionAction
typedAuctionValidator =
  mkTypedValidator
    $$(PlutusTx.compile [||wrap mkAuctionValidator||])
  where
    wrap ::
      forall d1 d2 d3 a.
      (PlutusTx.FromData d1, PlutusTx.FromData d2, PlutusTx.FromData d3) =>
      (d1 -> d2 -> d3 -> a) ->
      BuiltinData ->
      BuiltinData ->
      BuiltinData ->
      ()
    wrap v d1 d2 d3 =
      {- HLINT ignore "Evaluate" -}
      const () $
        v
          (fromMaybe (error ()) $ PlutusTx.fromBuiltinData d1)
          (fromMaybe (error ()) $ PlutusTx.fromBuiltinData d2)
          (fromMaybe (error ()) $ PlutusTx.fromBuiltinData d3)

-- wrapValidator @AuctionDatum @AuctionAction
-- out of scope

auctionValidator :: Validator
auctionValidator = (\(Versioned _ x) -> x) $ unTypedValidator typedAuctionValidator

-- TODO is there a named feild for this

auctionHash :: ValidatorHash
auctionHash = validatorHash typedAuctionValidator

auctionAddress :: Address
auctionAddress = scriptHashAddress $ ScriptHash $ (\(ValidatorHash v) -> v) auctionHash

data StartParams = StartParams
  { spDeadline :: !POSIXTime
  , spMinBid :: !Integer
  , spCurrency :: !CurrencySymbol
  , spToken :: !TokenName
  }
  deriving (Generic)

data BidParams = BidParams
  { bpCurrency :: !CurrencySymbol
  , bpToken :: !TokenName
  , bpBid :: !Integer
  }
  deriving (Generic)

data CloseParams = CloseParams
  { cpCurrency :: !CurrencySymbol
  , cpToken :: !TokenName
  }
  deriving (Generic)

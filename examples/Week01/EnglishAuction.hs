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
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Week01.EnglishAuction (
  Auction (..),
  StartParams (..),
  BidParams (..),
  CloseParams (..),
  -- , AuctionSchema
  -- , start, bid, close
  -- , endpoints
  -- , schemas
  -- , ensureKnownCurrencies
  -- , printJson
  -- , printSchemas
  -- , registeredKnownCurrencies
  -- , stage
) where

-- import           Control.Monad        hiding (fmap)
-- import           Data.Aeson           (ToJSON, FromJSON)
-- import           Data.List.NonEmpty   (NonEmpty (..))
-- import           Data.Map             as Map
-- import           Data.Text            (pack, Text)
import GHC.Generics (Generic)

-- import           Ledger               hiding (singleton)
-- import qualified Ledger.Constraints   as Constraints
-- import qualified Ledger.Typed.Scripts as Scripts
-- import           Ledger.Value         as Value
-- import           Ledger.Ada           as Ada
-- import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
-- import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types     (KnownCurrency (..))
-- import           Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (unless)
import Prelude qualified as P

-- import           Schema               (ToSchema)
-- import           Text.Printf          (printf)
-- new

-- , TxInfo

-- , TxOutRef

import Plutus.Model.V1 (Validator, txOutDatumHash)
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
  TxOut,
  Value,
 )

-- import Plutus.Model.V2 (TypedValidator)

-- TODO can we use psm here this?
import Plutus.Model.Validator (ValidatorHash)
import Plutus.Model.Validator.V1 (mkTypedValidator)

import Plutus.Model.V2 (TypedValidator, validatorHash)
import PlutusLedgerApi.V2 (TxInfo (txInfoInputs, txInfoValidRange), TxOut (txOutAddress), to)
import PlutusLedgerApi.V2.Contexts (ScriptContext (scriptContextTxInfo), TxInfo (txInfoOutputs), findDatum, getContinuingOutputs)
import PlutusLedgerApi.V2.Tx (TxOut (txOutValue))

-- some stuff seems to need to be v1 but other stuff seems to need to be v2
-- but they cause type errors to mix of course

import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (contains, from)

-- import Cardano.Api

lovelaceValueOf = _

-- TODO can't build plutus-ledger to import this

minLovelace :: Integer
minLovelace = 2000000

data Auction = Auction
  { aSeller :: !PubKeyHash
  , aDeadline :: !POSIXTime
  , aMinBid :: !Integer
  , aCurrency :: !CurrencySymbol
  , aToken :: !TokenName
  }
  deriving (P.Show, Generic)

instance Eq Auction where
  {-# INLINEABLE (==) #-}
  a == b =
    (aSeller a == aSeller b)
      && (aDeadline a == aDeadline b)
      && (aMinBid a == aMinBid b)
      && (aCurrency a == aCurrency b)
      && (aToken a == aToken b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bBidder :: !PubKeyHash
  , bBid :: !Integer
  }
  deriving (P.Show)

instance Eq Bid where
  {-# INLINEABLE (==) #-}
  b == c =
    (bBidder b == bBidder c)
      && (bBid b == bBid c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
  { adAuction :: !Auction
  , adHighestBid :: !(Maybe Bid)
  }
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

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
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
          Nothing -> False
          Just _ -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
       in
        case xs of
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
    (ownOutput, outputDatum) = case getContinuingOutputs $ error "TODO fix" ctx of
      [o] -> case txOutDatumHash $ error "TODO fix" o of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just ad' -> (error "TODO fix" o, ad')
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
        let
          os =
            [ o
            | o <- txInfoOutputs info
            , txOutAddress o == pubKeyHashAddress bBidder Nothing
            ]
         in
          case os of
            [o] -> txOutValue o == lovelaceValueOf bBid
            _ -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] =
          [ o'
          | o' <- txInfoOutputs info
          , txOutValue o' == v
          ]
       in
        txOutAddress o == pubKeyHashAddress h Nothing

typedAuctionValidator :: TypedValidator AuctionDatum AuctionAction
typedAuctionValidator =
  mkTypedValidator
    $$(PlutusTx.compile [||wrap mkAuctionValidator||])
  where
    wrap = _

-- wrapValidator @AuctionDatum @AuctionAction
-- out of scope

auctionValidator :: Validator
auctionValidator = validatorScript typedAuctionValidator

auctionHash :: ValidatorHash
auctionHash = validatorHash typedAuctionValidator

auctionAddress :: Address
auctionAddress = scriptHashAddress auctionHash

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

{-
type AuctionSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- ownPaymentPubKeyHash
    let a = Auction
                { aSeller   = pkh
                , aDeadline = spDeadline
                , aMinBid   = spMinBid
                , aCurrency = spCurrency
                , aToken    = spToken
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing
                }
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedAuctionValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started auction %s for token %s" (P.show a) (P.show v)

bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- ownPaymentPubKeyHash
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + bpBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
        bpBid
        (P.show adAuction)
        (P.show bpCurrency)
        (P.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    let t      = Value.singleton cpCurrency cpToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed auction %s for token (%s, %s)"
        (P.show adAuction)
        (P.show cpCurrency)
        (P.show cpToken)

findAuction :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn = do
    utxos <- utxosAt $ scriptHashAddress auctionHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@AuctionDatum{..}
                    | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "auction token missmatch"
        _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = awaitPromise (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" start
    bid'   = endpoint @"bid"   bid
    close' = endpoint @"close" close

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]

-}

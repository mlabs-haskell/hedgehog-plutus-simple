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

module Week01.Types where

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

import Plutus.Model.V2 (TypedValidator, unTypedValidator, validatorHash)
import PlutusLedgerApi.V2 (TxInfo (txInfoInputs, txInfoValidRange), TxOut (txOutAddress), to)
import PlutusLedgerApi.V2.Contexts (ScriptContext (scriptContextTxInfo), TxInfo (txInfoOutputs), findDatum, getContinuingOutputs)
import PlutusLedgerApi.V2.Tx (TxOut (txOutValue))

-- some stuff seems to need to be v1 but other stuff seems to need to be v2
-- but they cause type errors to mix of course

import Cardano.Simple.PlutusLedgerApi.V1.Scripts (ValidatorHash (..))
import Plutus.Model.V2 (Versioned (..))
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (contains, from)
import PlutusLedgerApi.V2 (ToData (toBuiltinData))

-- GHC stage restriction forces this to be in a seperate module

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

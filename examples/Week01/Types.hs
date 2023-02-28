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

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName,
 )
import PlutusTx qualified
import PlutusTx.Prelude hiding (unless)
import Prelude qualified as P

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
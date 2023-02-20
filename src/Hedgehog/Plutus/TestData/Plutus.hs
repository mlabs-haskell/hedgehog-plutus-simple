{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hedgehog.Plutus.TestData.Plutus where

import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog.Plutus.Generics (Simple (Simple))
import Hedgehog.Plutus.TestData (TestData)

deriving via (Simple Plutus.TxOutRef) instance TestData Plutus.TxOutRef
deriving via (Simple Plutus.PubKeyHash) instance TestData Plutus.PubKeyHash
deriving via (Simple Plutus.Value) instance TestData Plutus.Value
deriving via (Simple Plutus.Address) instance TestData Plutus.Address

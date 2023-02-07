{-# LANGUAGE ImpredicativeTypes #-}

module Hedgehog.Plutus.ScriptContext where

import Data.Kind (Type)

data Purpose = Spending Type | Minting

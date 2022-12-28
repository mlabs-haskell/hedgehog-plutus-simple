module Hedgehog.Plutus.Objective where

import Data.Map (Map)

import PlutusLedgerApi.V2 qualified as Plutus

import Hedgehog qualified

import Hedgehog.Plutus.TxSequence

data Objective = Objective
  { interval ::
      Map
        Plutus.CurrencySymbol
        (Map Plutus.TokenName (Maybe Integer, Maybe Integer))
  , runObjective :: Plutus.Value -> Hedgehog.Gen TxBuilder
  }

genObjective :: Objective -> Hedgehog.Gen TxBuilder
genObjective Objective {interval, runObjective} = _ >>= runObjective

module Hedgehog.Plutus.TestData where

import Prelude hiding (id, (.))

import Control.Category ((.))

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Tx

data ScriptTx = ScriptTx
  { scriptTxPurpose :: ScriptPurpose
  , scriptTx :: Tx 'Unbalanced
  -- ^ The remainder of the transaction, not including the purposes's mint/spend
  }

newtype Bad a = Bad {unBad :: a}

class TxTest gen where
  data Good gen

  txAdjunction :: TxContext -> Adjunction gen ScriptTx

  validate :: TxContext -> gen -> Maybe (Good gen)

  generalise :: Good gen -> gen

txTestAdjunction ::
  forall gen.
  (TxTest gen) =>
  TxContext ->
  Adjunction (Either (Bad gen) (Good gen)) ScriptTx
txTestAdjunction txc =
  txAdjunction txc
    . Adjunction
      { lower = either unBad generalise
      , raise = \gen -> maybe (Left (Bad gen)) Right (validate txc gen)
      }

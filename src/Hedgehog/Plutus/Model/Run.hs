module Hedgehog.Plutus.Model.Run (
  submitTx,
) where

import Data.Functor (($>))
import Data.Map qualified as Map

import Hedgehog.Plutus.Model.Internal (
  Balanced,
  BalancedTx (BalancedTx),
  TxContext (mockchain),
  getTx,
  toSimpleModelTx,
 )

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Ledger.Tx qualified as Fork

submitTx :: BalancedTx -> TxContext -> Maybe TxContext
submitTx (BalancedTx tx) ctx =
  (\m -> ctx {mockchain = m})
    <$> submitModelTx (toSimpleModelTx @'True ctx tx) (mockchain ctx)

submitModelTx :: Balanced Model.Tx -> Model.Mock -> Maybe Model.Mock
submitModelTx (getTx @'True -> (tx :: Model.Tx)) =
  uncurry ($>)
    . Model.runMock (Model.submitTx pkh tx >> Model.checkErrors)
  where
    pkh =
      case Map.keys $ Fork.txSignatures $ Model.tx'plutus tx of
        (pkh : _) -> pkh
        [] -> error "tried to submit a transaction with no signatures"

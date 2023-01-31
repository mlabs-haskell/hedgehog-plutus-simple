module Hedgehog.Plutus.Model.Run (
  submitTx,
  submitBalancedTx,
) where

import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

import Hedgehog.Plutus.Model.Internal (
  Balanced,
  BalancedTx (BalancedTx),
  Tx,
  TxContext (mockchain),
  balanceTx,
  getTx,
  toSimpleModelTx,
 )

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Ledger.Tx qualified as Fork

import Hedgehog (
  PropertyT,
  forAll,
 )

{- | balances and submits a transaction
 returns a PropertyT m TxContext because the balance is nondeterministic
 and the balance and submision can fail
-}
submitTx :: Monad m => Tx -> TxContext -> PropertyT m TxContext
submitTx tx ctx = do
  btx <- forAll $ fromMaybe (fail "failed to balance") $ balanceTx ctx tx
  maybe (fail "failed to submit") pure (submitBalancedTx btx ctx)

-- TODO both of these functions should probably return better errors
-- which can then be logged better here

-- | submits a balanced tx reutrns a maybe because this
submitBalancedTx :: BalancedTx -> TxContext -> Maybe TxContext
submitBalancedTx (BalancedTx tx) ctx =
  (\m -> ctx {mockchain = m})
    <$> submitModelTx (toSimpleModelTx @'True ctx tx) (mockchain ctx)

submitModelTx :: Balanced Model.Tx -> Model.Mock -> Maybe Model.Mock
submitModelTx (getTx @'True -> tx :: Model.Tx) =
  case Map.keys $ Fork.txSignatures $ Model.tx'plutus tx of
    (pkh : _) ->
      uncurry ($>)
        . Model.runMock (Model.submitTx pkh tx >> Model.checkErrors)
    -- TODO does which key matter?
    -- should we invoke Gen to chose?
    [] -> const Nothing

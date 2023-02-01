module Hedgehog.Plutus.Model.Run (
  submitTx,
  submitBalancedTx,
) where

import Data.Map qualified as Map

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
  btx <- either (fail . ("Balance error: " <>)) forAll (balanceTx ctx tx)
  either (fail . (<> "Submit error: ")) pure (submitBalancedTx btx ctx)

-- TODO both of these functions should probably return better errors
-- which can then be logged better here

-- | submits a balanced tx reutrns a maybe because this
submitBalancedTx :: BalancedTx -> TxContext -> Either String TxContext
submitBalancedTx (BalancedTx tx) ctx =
  (\m -> ctx {mockchain = m})
    <$> submitModelTx (toSimpleModelTx @'True ctx tx) (mockchain ctx)

submitModelTx :: Balanced Model.Tx -> Model.Mock -> Either String Model.Mock
submitModelTx (getTx @'True -> tx :: Model.Tx) mock =
  case Map.keys $ Fork.txSignatures $ Model.tx'plutus tx of
    [] -> Left "no public keys in tx"
    (pkh : _) ->
      case Model.runMock (Model.submitTx pkh tx >> Model.checkErrors) mock of
        (Just err, _) -> Left err
        (Nothing, mock) -> Right mock

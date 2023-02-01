module Hedgehog.Plutus.Model.Run (
  submitTx,
  submitTxAs,
  submitBalancedTx,
) where

import Data.Map qualified as Map

import Hedgehog.Plutus.Model.Internal (
  Balanced,
  BalancedTx (BalancedTx),
  Tx,
  TxContext (mockchain),
  balanceTx,
  balanceTxAsPubKey,
  getTx,
  toSimpleModelTx,
 )

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Ledger.Tx qualified as Fork

import Hedgehog (
  PropertyT,
  forAll,
 )
import PlutusLedgerApi.V1 (PubKeyHash)

{- | balances and submits a transaction
 returns a PropertyT m TxContext because the balance is nondeterministic
 and the balance and submision can fail
 in either case it fails the test in the PropertyT monad
-}
submitTx :: Monad m => Tx -> TxContext -> PropertyT m TxContext
submitTx tx ctx = do
  btx <- either (fail . ("Balance error: " <>) . show) forAll (balanceTx ctx tx)
  either (fail . (<> "Submit error: ") . show) pure (submitBalancedTx btx ctx)

-- | as submitTx but the transaction balance is restricted to a single PubKeyHash
submitTxAs :: Monad m => PubKeyHash -> Tx -> TxContext -> PropertyT m TxContext
submitTxAs pkh tx ctx = do
  btx <- either (fail . ("Balance error: " <>) . show) forAll (balanceTxAsPubKey ctx tx pkh)
  either (fail . (<> "Submit error: ") . show) pure (submitBalancedTx btx ctx)

-- | submits a balanced tx
submitBalancedTx :: BalancedTx -> TxContext -> Either SubmitError TxContext
submitBalancedTx (BalancedTx tx) ctx =
  (\m -> ctx {mockchain = m})
    <$> submitModelTx (toSimpleModelTx @'True ctx tx) (mockchain ctx)

submitModelTx :: Balanced Model.Tx -> Model.Mock -> Either SubmitError Model.Mock
submitModelTx (getTx @'True -> tx :: Model.Tx) mock =
  case Map.keys $ Fork.txSignatures $ Model.tx'plutus tx of
    [] -> Left NoPubKeys
    (pkh : _) ->
      case Model.runMock (Model.submitTx pkh tx >> Model.checkErrors) mock of
        (Just err, _) -> Left $ PSMSubmitError err
        (Nothing, mock) -> Right mock

data SubmitError
  = PSMSubmitError String
  | NoPubKeys
  deriving stock (Eq, Show)

module Spec.LiquidityBridge.PaymentConfirmation (tests) where

import Cardano.Prelude (fmap, mempty, uncurry)
import Control.Monad (void)
import Data.Map.Strict (keys)
import Data.Map.Strict qualified as Map
import Data.Set (singleton)
import Data.String.Compat (String)
import Ledger (PubKeyHash, Tx (..), TxOut (..), txId)
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Contexts (pubKeyHash)
import Ledger.Interval qualified as Interval
import LiquidityBridge.PaymentConfirmation (
  makeEthValue,
  makeValues,
  paymentConfirmationEthFromPubKey,
  paymentConfirmationFromPubKey,
  policy,
 )
import LiquidityBridge.Schema (LiquidityBridgeSchema, PCEParams (..), PCParams (..))
import Plutus.Contract (
  Contract,
  ContractError,
  UtxoMap,
  awaitTxConfirmed,
  ownPubKey,
  submitUnbalancedTx,
  utxoAt,
 )
import Plutus.Contract.Test (
  TracePredicate,
  Wallet (..),
  assertContractError,
  assertDone,
  checkPredicate,
  walletFundsChange,
  walletPubKey,
  (.&&.),
 )
import Plutus.Trace.Emulator as Trace
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Scripts (unitRedeemer)
import Plutus.V1.Ledger.Tx (
  RedeemerPtr (..),
  ScriptTag (Mint),
  pubKeyTxIn,
  txOutTxOut,
 )
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Prelude hiding (fmap, mempty, uncurry)
import Test.Tasty
import Wallet.API (always)
import Wallet.Emulator.Wallet (walletAddress)

-- | Get a wallets PubKeyHash
walletPkh :: Wallet -> PubKeyHash
walletPkh = pubKeyHash . walletPubKey

-- | Pseudo centralised server Wallet
serverWallet :: Wallet
serverWallet = Wallet 1

serverWalletPkh :: PubKeyHash
serverWalletPkh = walletPkh serverWallet

clientWallet :: Wallet
clientWallet = Wallet 2

-- | Parameters to PaymentConfirmation sending tokens to given Wallet
pcParams :: Wallet -> PCParams
pcParams w =
  PCParams
    { ppRecipient = walletPkh w
    , ppToken0Amount = 5
    , ppToken0Name = "myToken1"
    , ppToken0Address = "a"
    , ppToken1Amount = 3
    , ppToken1Name = "myToken2"
    , ppToken1Address = "b"
    , ppDexAddress = walletPkh w -- This isn't used yet, so placeholder address used
    }

-- | Parameters to PaymentConfirmation sending tokens to given Wallet
pceParams :: Wallet -> PCEParams
pceParams w =
  PCEParams
    { pepRecipient = walletPkh w
    , pepAmount = 10
    }

-- Params Wallet here doesnt matter, discarded by makeValue

-- | Expected payment confirmation values to mint
pcVals :: (Value, Value)
pcVals = makeValues serverWalletPkh $ pcParams clientWallet

-- | Combined payment confirmation value
pcCombinedVal :: Value
pcCombinedVal = uncurry (<>) pcVals

-- | Expected payment confirmation eth value to mint
pceVal :: Value
pceVal = makeEthValue serverWalletPkh $ pceParams clientWallet

{- | Contract for attempting a PaymentConfirmation (parametised to server wallet)
 and sending the minted tokens to given Wallet
-}
pcContract :: Wallet -> Contract () LiquidityBridgeSchema ContractError ()
pcContract w = paymentConfirmationFromPubKey serverWalletPkh $ pcParams w

{- | Contract for attempting a PaymentConfirmationEth (parametised to server wallet)
 and sending the minted tokens to given Wallet
-}
pceContract :: Wallet -> Contract () LiquidityBridgeSchema ContractError ()
pceContract w = paymentConfirmationEthFromPubKey serverWalletPkh $ pceParams w

directTx :: UtxoMap -> Tx
directTx utxoMap =
  Tx
    { txInputs = singleton $ pubKeyTxIn $ head $ keys utxoMap
    , txCollateral = mempty
    , txOutputs = [TxOut (walletAddress clientWallet) (fst pcVals) Nothing]
    , txMint = fst pcVals
    , txFee = mempty
    , txValidRange = always
    , txMintScripts = singleton $ policy serverWalletPkh (pcParams clientWallet).ppToken0Address
    , txSignatures = mempty
    , txRedeemers = Map.singleton (RedeemerPtr Mint 0) unitRedeemer
    , txData = mempty
    }

packUnbalancedTx :: Tx -> PubKeyHash -> UtxoMap -> UnbalancedTx
packUnbalancedTx tx pkh utxos =
  UnbalancedTx
    { unBalancedTxTx = tx
    , unBalancedTxRequiredSignatories = singleton pkh
    , unBalancedTxUtxoIndex = fmap txOutTxOut utxos
    , unBalancedTxValidityTimeRange = Interval.always
    }

directContract :: Contract () LiquidityBridgeSchema ContractError ()
directContract = do
  pkh <- pubKeyHash <$> ownPubKey
  utxoMap <- utxoAt $ pubKeyHashAddress pkh
  let tx = directTx utxoMap
  ledgerTx <- submitUnbalancedTx $ packUnbalancedTx tx pkh utxoMap
  void $ awaitTxConfirmed $ txId ledgerTx

makePredicate ::
  -- | Contract to run
  Contract () LiquidityBridgeSchema ContractError () ->
  -- | Wallet to run contract from
  Wallet ->
  -- | Value expected in client wallet
  Value ->
  -- | Error string
  String ->
  -- | Should succeed
  Bool ->
  TracePredicate
makePredicate ct w v err succeed =
  makeAssertPredicate succeed ct (Trace.walletInstanceTag w) err
    .&&. walletFundsChange clientWallet v
    .&&. walletFundsChange serverWallet mempty

-- Types for (const True) for assertDone and assertContractError are slightly different
-- so we need a bit of repeated code, sadly
makeAssertPredicate ::
  Bool ->
  Contract () LiquidityBridgeSchema ContractError () ->
  Trace.ContractInstanceTag ->
  String ->
  TracePredicate
makeAssertPredicate True ct tag = assertDone ct tag $ const True
makeAssertPredicate False ct tag = assertContractError ct tag $ const True

{- | Tests for PaymentConfirmationSundae endpoint

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "PaymentConfirmation"
    [ -- Does a normal confirmation validation from the server, minting and sending tokens
      --   to a public key address wallet
      checkPredicate
        "Valid deposit confirmation"
        (makePredicate (pcContract clientWallet) serverWallet pcCombinedVal "Contract failed unexpectedly" True)
        (pcContractTrace serverWallet clientWallet)
    , -- Attempts a confirmation from a non-server key, expected to fail
      checkPredicate
        "Invalid deposit from non-server wallet"
        (makePredicate (pcContract clientWallet) clientWallet mempty "Contract incorrectly succeeded" False)
        (pcContractTrace clientWallet clientWallet)
    , -- Does an eth confirmation validation from the server, minting and sending tokens
      --   to a public key address wallet
      checkPredicate
        "Valid eth deposit confirmation"
        (makePredicate (pceContract clientWallet) serverWallet pceVal "Contract failed unexpectedly" True)
        (pceContractTrace serverWallet clientWallet)
    , -- Attempts a confirmation from a non-server key, expected to fail
      checkPredicate
        "Invalid eth deposit from non-server wallet"
        (makePredicate (pceContract clientWallet) clientWallet mempty "Contract incorrectly succeeded" False)
        (pceContractTrace clientWallet clientWallet)
    , -- Mints tokens via the policy constructing the transaction directly
      checkPredicate
        "Direct valid minting"
        (makePredicate directContract serverWallet (fst pcVals) "Policy failed unexpectedly" True)
        (directTrace serverWallet)
    , -- Attempts to mint tokens directly via the policy, from the client.
      checkPredicate
        "Invalid direct deposit from non-server wallet"
        (makePredicate directContract clientWallet mempty "Policy incorrectly succeeded" False)
        (directTrace clientWallet)
    ]

-- | EmulatorTrace for calling the PaymentConfirmation endpoint
pcContractTrace ::
  -- | Wallet calling the endpoint
  Wallet ->
  -- | Address to receive the tokens
  Wallet ->
  Trace.EmulatorTrace ()
pcContractTrace from to = do
  _ <- Trace.activateContractWallet from $ pcContract to
  void $ Trace.waitNSlots 5

-- | EmulatorTrace for calling the PaymentConfirmationEth endpoint
pceContractTrace ::
  -- | Wallet calling the endpoint
  Wallet ->
  -- | Address to receive the tokens
  Wallet ->
  Trace.EmulatorTrace ()
pceContractTrace from to = do
  _ <- Trace.activateContractWallet from $ pceContract to
  void $ Trace.waitNSlots 5

directTrace :: Wallet -> Trace.EmulatorTrace ()
directTrace from = do
  _ <- Trace.activateContractWallet from directContract
  void $ Trace.waitNSlots 5

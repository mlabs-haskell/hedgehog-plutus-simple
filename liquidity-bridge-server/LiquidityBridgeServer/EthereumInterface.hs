module LiquidityBridgeServer.EthereumInterface (
  ethereumSetup,
) where

import Control.Monad (forM_, when)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Either (partitionEithers)
import Data.Map (keys, notMember, (!))
import Data.Text (Text, pack, unpack)
import Ledger (PubKeyHash (..))
import LiquidityBridge.Schema (PCEParams (..), PCParams (..))
import LiquidityBridgeServer.CardanoInterface qualified as CI
import LiquidityBridgeServer.CardanoRequests qualified as CR
import LiquidityBridgeServer.Config (Config)
import LiquidityBridgeServer.EthereumLogData (DepositEthEventParams, DepositEventParams)
import LiquidityBridgeServer.EthereumRequests qualified as ER
import LiquidityBridgeServer.EthereumTypes qualified as ET
import Plutus.PAB.Events.Contract (ContractInstanceId)
import Plutus.V1.Ledger.Value (TokenName (..))
import PlutusTx.Builtins qualified as Builtins
import Text.Printf (printf)
import Wallet.Emulator.Wallet (Wallet (..))
import Prelude hiding (lookup, map)

-- Event signature for LP deposits
depositEvent :: ByteString
depositEvent = "Deposit(address,bytes28,bytes32,string,string,address,address,uint256,uint256)"

-- Event signature for eth deposits
depositEthEvent :: ByteString
depositEthEvent = "DepositEth(address,bytes28,uint256)"

-- Method signature for config updates
updateConfigMethod :: ByteString
updateConfigMethod = "updateConfig(bytes32[])"

{- | Removes the quotes added by encode

 @since 0.1
-}
removeQuotes :: String -> String
removeQuotes = init . tail

{- | Sends transactions to all ethereum contracts updating them with the cardano dex list

 @since 0.1
-}
updateAcceptedDexes :: ER.EthReceiver -> Config -> IO ()
updateAcceptedDexes ethRc config = do
  putStrLn "Cardano dex config changed, updating ethereum contracts..."

  let names :: [String]
      names = keys config.cardanoDexMap

      sig = ER.methodHash updateConfigMethod
      -- Hard coded as the next value, as this method only takes 1 argument
      arrayLocation = ET.padLeft 64 '0' "20"
      arrayLength = ET.padLeft 64 '0' $ show $ length names
      arrayData = concatMap strToBytes32 names
      txData = sig ++ arrayLocation ++ arrayLength ++ arrayData

      makeTx :: String -> ET.EthOutTransaction
      makeTx targetContract =
        ET.EthOutTransaction
          { eotFrom = pack config.ethCentralWalletAddress
          , eotTo = pack targetContract
          , -- TODO: Change this to a more reasonable value before real net use.
            -- ERC20 transfer rarely go above ~150k, we're doing less but scaling with input.
            -- ~ 200k would be enough for about 10 dexes.
            -- This could also scale relative to dex count.
            eotGas = ET.EthNumber 500000
          , eotData = pack txData
          }
      txs :: [ET.EthOutTransaction]
      txs = makeTx <$> config.ethContractAddresses

  -- Send the transactions
  (txrErrs, txrs) <- partitionEithers <$> mapM (ER.sendEthTransaction ethRc) txs
  forM_ txrErrs $ \err -> do
    putStrLn "Failed to update an ethereum contract:"
    print err

  -- Log any failed receipts
  let failedTxrs = filter ((== False) . ET.getEthBool . ET.etrStatus) txrs
  forM_ failedTxrs $ \txr ->
    putStrLn $ "Failed to update ethereum contract at address " ++ unpack txr.etrTo

  when (null failedTxrs) $ putStrLn "Updated all configs successfully."
  where
    -- Encode a string into bytes using the EthString encoding
    strToBytes32 :: String -> String
    strToBytes32 = drop 2 . removeQuotes . toString . encode . ET.EthString . ET.padLeft 32 '\NUL'

{- | Starts up the ethereum loop and subscribes to transfers to server wallet on LP ERC20 contract

 @since 0.1
-}
ethereumSetup :: Config -> ER.EthReceiver -> CR.CardReceiver -> IO ()
ethereumSetup config ethRc cardRc = do
  -- If config changes, we need to update the lists on the solidity contracts
  when (config.cardanoDexMapChanged || config.manualDexUpdate) $ updateAcceptedDexes ethRc config

  ER.catchEthEvents config ethRc cardRc depositEvent handleLPTransfer
  ER.catchEthEvents config ethRc cardRc depositEthEvent handleEthEvent

serverWallet :: Wallet
serverWallet = Wallet 1

{- | Handler for deposit event on the ethereum contract

 @since 0.1
-}
handleLPTransfer :: Config -> ER.EthReceiver -> CR.CardReceiver -> ET.EthLogEvent DepositEventParams -> IO ()
handleLPTransfer config _ cardRc evt
  | notMember (unpack evt.eleData.dpDexName) config.cardanoDexMap =
    putStrLn "Nonexistant dex name"
  | otherwise = do
    let fromAddress :: Text
        fromAddress = evt.eleData.dpSender
        toAddress :: PubKeyHash
        toAddress = evt.eleData.dpCardanoAddress
        dexAddress :: PubKeyHash
        dexAddress = config.cardanoDexMap ! (unpack evt.eleData.dpDexName)
        pcParams :: PCParams
        pcParams =
          PCParams
            { ppRecipient = toAddress
            , ppDexAddress = dexAddress
            , ppToken0Amount = evt.eleData.dpToken0Amount
            , ppToken0Name = TokenName $ Builtins.encodeUtf8 $ Builtins.toBuiltin evt.eleData.dpToken0Name
            , ppToken0Address = evt.eleData.dpToken0Address
            , ppToken1Amount = evt.eleData.dpToken1Amount
            , ppToken1Name = TokenName $ Builtins.encodeUtf8 $ Builtins.toBuiltin evt.eleData.dpToken1Name
            , ppToken1Address = evt.eleData.dpToken1Address
            }

    putStrLn $
      printf
        "Caught transfer of %d %s's and %d %s's to server wallet from %s to %s."
        evt.eleData.dpToken0Amount
        evt.eleData.dpToken0Name
        evt.eleData.dpToken1Amount
        evt.eleData.dpToken1Name
        fromAddress
        (show toAddress)
    putStrLn "Forwarding event to cardano"

    eCIID <- CI.callCardanoEndpoint @ContractInstanceId config "PaymentConfirmation" pcParams serverWallet

    CI.handleCardanoInstance cardRc eCIID

{- | Handler for deposit eth event on the ethereum contract

 @since 0.1
-}
handleEthEvent :: Config -> ER.EthReceiver -> CR.CardReceiver -> ET.EthLogEvent DepositEthEventParams -> IO ()
handleEthEvent config _ cardRc evt = do
  let pceParams :: PCEParams
      pceParams =
        PCEParams
          { pepRecipient = evt.eleData.depCardanoAddress
          , pepAmount = evt.eleData.depAmount
          }
  putStrLn $
    printf
      "Caught transfer of %d Eth/WEth to server wallet from %s to %s."
      evt.eleData.depAmount
      evt.eleData.depSender
      (show evt.eleData.depCardanoAddress)
  putStrLn "Forwarding event to cardano"

  eCIID <- CI.callCardanoEndpoint @ContractInstanceId config "PaymentConfirmationEth" pceParams serverWallet

  CI.handleCardanoInstance cardRc eCIID

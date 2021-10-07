{-# LANGUAGE TypeFamilies #-}

module LiquidityBridgeServer.EthereumRequests (
  EthReceiver,
  EthSubscription (..),
  catchEthEvents,
  ethHash,
  methodHash,
  sendEthRequest,
  sendEthTransaction,
  sendEthTransactionBackground,
) where

import Control.Concurrent (
  ThreadId,
  forkIO,
  killThread,
  putMVar,
  takeMVar,
  threadDelay,
 )

import Control.Monad (void, when)
import Crypto.Hash (Keccak_256 (..), hashWith)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Vector (fromList)
import LiquidityBridgeServer.CardanoRequests qualified as CR
import LiquidityBridgeServer.Config (Config)
import LiquidityBridgeServer.EthereumTypes (
  EthError (..),
  EthMessage (..),
  EthNumber (..),
  EthOutTransaction (..),
  EthRequestParams (..),
  EthResponse,
  EthSubscribedEvent,
  EthTransactionReceipt (..),
  ethGetReceipt,
  ethOutTransaction,
  unsubscribe,
 )
import LiquidityBridgeServer.EthereumTypes qualified as ET
import LiquidityBridgeServer.Requests (
  Receiver (..),
  ReceiverData (..),
  ReceiverSubscription (..),
  addListener,
  waitLoop,
 )
import LiquidityBridgeServer.Requests qualified as R
import Network.WebSockets (sendTextData)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Prelude

{- | Formalised subscription with its ID and the reader threads ID

 @since 0.1
-}
data EthSubscription = EthSubscription
  { esThreadId :: ThreadId
  , esSubscriptionId :: EthNumber
  }
  deriving stock (Eq, Show)

instance ReceiverSubscription EthSubscription where
  type ReceiverError EthSubscription = EthError
  type ReceiverRequest EthSubscription = EthRequestParams
  destroySubscriber = removeEthSubscriber
  createSubscriber = addEthSubscriber

type EthReceiver = Receiver EthSubscription

{- | Kills a subscription listener, ends the eth subscription, and removes the subscription from the receiver

 @since 0.1
-}
removeEthSubscriber :: EthReceiver -> EthSubscription -> IO ()
removeEthSubscriber rc sub = do
  killThread sub.esThreadId
  void $ sendEthRequest @Bool rc $ unsubscribe sub.esSubscriptionId

{- | Sends a websocket transaction, halting until the connection returns a message with the same ID
  Effectively treats a websocket request like a synchronous http request

 @since 0.1
-}
sendEthRequest ::
  forall (response :: Type) (request :: Type).
  (ToJSON request, FromJSON response) =>
  EthReceiver ->
  EthRequestParams request ->
  IO (Either EthError response)
sendEthRequest rc req = do
  recData <- takeMVar rc.rMVar
  let i = recData.rdMsgIdCounter
  sendTextData rc.rConnection $ encode $ EthMessage req i
  putMVar rc.rMVar $ recData {rdMsgIdCounter = i + 1}
  waitLoop rc $ checkEthRequestResponse i

{- | Validator for a eth transactions response

 @since 0.1
-}
checkEthRequestResponse ::
  forall (response :: Type).
  FromJSON response =>
  Integer ->
  ByteString ->
  Maybe (Either EthError response)
checkEthRequestResponse i str = do
  val <- decode @(EthResponse response) str
  if emId val == i
    then return val.emContents.eResResult
    else Nothing

{- | Asynconously sends an eth tranaction and runs a callback on completion or failure

 @since 0.1
-}
sendEthTransactionBackground ::
  EthReceiver ->
  EthOutTransaction ->
  (Either EthError EthTransactionReceipt -> IO ()) ->
  IO ThreadId
sendEthTransactionBackground rc tx f = forkIO $ do
  res <- sendEthTransaction rc tx
  f res

{- | Sends an eth transaction and busy waits for the receipt

 @since 0.1
-}
sendEthTransaction ::
  EthReceiver ->
  EthOutTransaction ->
  IO (Either EthError EthTransactionReceipt)
sendEthTransaction rc tx = do
  eHash <- sendEthRequest rc (ethOutTransaction tx)
  either (return . Left) hashLoop eHash
  where
    hashLoop :: Text -> IO (Either EthError EthTransactionReceipt)
    hashLoop hash = do
      emReceipt <- sendEthRequest @(Maybe EthTransactionReceipt) rc $ ethGetReceipt hash
      flip (either $ return . Left) emReceipt $
        maybe (threadDelay 500_000 >> hashLoop hash) (return . Right)

{- | Subscribes to an event on eth, adds a listener to the responses, stores the subscription in the receiver

 @since 0.1
-}
addEthSubscriber ::
  forall (response :: Type) (request :: Type).
  (ToJSON request, FromJSON response) =>
  EthReceiver ->
  EthRequestParams request ->
  (response -> IO ()) ->
  IO (Either EthError EthSubscription)
addEthSubscriber rc req callback = do
  ret <- sendEthRequest @EthNumber rc req
  case ret of
    Left s -> return $ Left s
    Right subId -> do
      tId <- addListener @response rc (checkSubscriptionResponse subId) callback
      return $ Right $ EthSubscription tId subId

{- | Validator for a eth subscription responses

 @since 0.1
-}
checkSubscriptionResponse ::
  forall (response :: Type).
  FromJSON response =>
  EthNumber ->
  ByteString ->
  Maybe (Either EthError response)
checkSubscriptionResponse subId str = do
  val <- decode @(EthSubscribedEvent response) str
  if val.emContents.eReqParams.eResSubscription == Just subId
    then return val.emContents.eReqParams.eResResult
    else Nothing

{- | Reads the current block number from file (Config.hs's ethBlockNumberFile), defaulting to 0

 @since 0.1
-}
readBlockMarker :: Config -> ByteString -> IO (Integer, Integer)
readBlockMarker config eventSig = do
  let path = config.ethBlockMarkerPrefix ++ ethHash eventSig
  exists <- doesFileExist path
  if not exists
    then return (0, 0)
    else do
      txt <- readFile path
      return $ fromMaybe (0, 0) $ readMaybe @(Integer, Integer) txt

{- | Writes a block number to file (Config.hs's ethBlockNumberFile)

 @since 0.1
-}
writeBlockMarker :: Config -> ByteString -> (Integer, Integer) -> IO ()
writeBlockMarker config eventSig = writeFile (config.ethBlockMarkerPrefix ++ ethHash eventSig) . show

{- | Calls a handler for an event, but only if it has not already been handled

 @since 0.1
-}
wrapHandler ::
  forall (response :: Type).
  Config ->
  ByteString ->
  (ET.EthLogEvent response -> IO ()) ->
  ET.EthLogEvent response ->
  IO ()
wrapHandler config eventSig f evt = do
  (cBlockNumber, cTxNumber) <- readBlockMarker config eventSig
  let blockNumber :: Integer
      blockNumber = evt.eleBlockNumber.getEthNumber
      txNumber :: Integer
      txNumber = evt.eleTransactionIndex.getEthNumber

  when (cBlockNumber < blockNumber || cTxNumber < txNumber) $ do
    f evt
    writeBlockMarker config eventSig (blockNumber, txNumber)

ethHash :: ByteString -> String
ethHash = ("0x" ++) . show . hashWith Keccak_256 . toStrict

methodHash :: ByteString -> String
methodHash = ("0x" ++) . take 8 . drop 2 . ethHash

{- | Filter for transfer events to server wallet on LP ERC20 contract

 @since 0.1
-}
lpFilter :: Config -> ByteString -> Maybe Integer -> ET.EthLogsFilter
lpFilter config eventSig mBlock =
  ET.ethLogsFilter
    { ET.elfFromBlock = ET.EthNumber <$> mBlock
    , ET.elfAddress =
        -- We only watch our own ethereum contracts
        Just $ fromList $ pack <$> config.ethContractAddresses
    , ET.elfTopics = Just $ fromList [ET.EthLogFilterTopicSingle $ pack $ ethHash eventSig]
    }

{- | Takes various details needed for a handler, runs the handler on new event and past events

 @since 0.1
-}
catchEthEvents ::
  forall (response :: Type).
  FromJSON response =>
  Config ->
  EthReceiver ->
  CR.CardReceiver ->
  ByteString ->
  (Config -> EthReceiver -> CR.CardReceiver -> ET.EthLogEvent response -> IO ()) ->
  IO ()
catchEthEvents config ethRc cardRc eventSig handler = do
  (blockNumber, _) <- readBlockMarker config eventSig

  -- We need to subscribe to transfer events on the LP smart contract for updates (ethereum)
  putStrLn $ toString $ "Subscribing to event: " <> eventSig
  R.printOnError $
    R.addSubscriber @(ET.EthLogEvent response)
      ethRc
      (ET.subscribeLogs $ lpFilter config eventSig Nothing)
      (wrapHandler config eventSig $ handler config ethRc cardRc)

  -- We need to check for events that we missed
  let getLogsMsg = ET.getLogs . lpFilter config eventSig . Just $ blockNumber
  eLogs <- sendEthRequest @[ET.EthLogEvent response] ethRc getLogsMsg
  case eLogs of
    Right logs@(_ : _) -> do
      putStrLn $ "Found " ++ show (length logs) ++ " potentially missed events, handling..."
      mapM_ (wrapHandler config eventSig $ handler config ethRc cardRc) logs
      putStrLn "Finished."
    _ -> putStrLn "No missed events found."

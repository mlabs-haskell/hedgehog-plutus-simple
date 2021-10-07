module LiquidityBridgeServer.CardanoInterface (
  callCardanoEndpoint,
  handleCardanoInstance,
) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import LiquidityBridgeServer.CardanoRequests qualified as CR
import LiquidityBridgeServer.Config (Config)
import LiquidityBridgeServer.Requests qualified as R
import Network.HTTP.Simple (
  Request,
  getResponseBody,
  httpJSONEither,
  parseRequest,
  setRequestBodyJSON,
  setRequestMethod,
 )
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), InstanceStatusToClient (..))
import System.Timeout (timeout)
import Wallet.Emulator.Wallet (Wallet (..))
import Prelude

{- | Calls an endpoint on the PAB simulator from a wallet

 @since 0.1
-}
callCardanoEndpoint ::
  forall (response :: Type) (request :: Type).
  (ToJSON request, FromJSON response) =>
  Config ->
  Text ->
  request ->
  Wallet ->
  IO (Either String response)
callCardanoEndpoint config endpoint reqData w = do
  let body :: ContractActivationArgs (CR.CardMessage request)
      body = ContractActivationArgs (CR.CardMessage reqData endpoint) w
      makeReq :: Request -> Request
      makeReq =
        setRequestBodyJSON body
          . setRequestMethod "POST"
      url :: String
      url = "http://" ++ config.cardanoEndpoint.url ++ ":" ++ show config.cardanoEndpoint.port ++ "/api/contract/activate"
  rawReq <- parseRequest url
  meRes <- timeout 5_000_000 $ httpJSONEither (makeReq rawReq)
  return $ maybe (Left "Timed out") (either (Left . show) Right . getResponseBody) meRes

{- | Observes a cardano ContractInstanceId and awaits a ContractFinished event, printing relevant information

 @since 0.1
-}
handleCardanoInstance :: CR.CardReceiver -> Either String ContractInstanceId -> IO ()
handleCardanoInstance cardRc eCIID = do
  flip (either $ putStrLn . ("Failed to send: " ++)) eCIID $ \ciID -> do
    putStrLn $ "Subscribing to contract instance: " ++ show (unContractInstanceId ciID)
    subMVar <- newEmptyMVar @CR.CardSubscription
    eSub <-
      R.addSubscriber @(ContractInstanceId, InstanceStatusToClient)
        cardRc
        (CR.subscribeContractInstance ciID)
        (handleInstanceChange cardRc subMVar)
    either return (putMVar subMVar) eSub

{- | Handler for `handleCardanoInstance`s subscription

 @since 0.1
-}
handleInstanceChange :: CR.CardReceiver -> MVar CR.CardSubscription -> (ContractInstanceId, InstanceStatusToClient) -> IO ()
handleInstanceChange cardRc mvar (ciID, ContractFinished mErr) = do
  putStrLn $ case mErr of
    Nothing -> "Contract instance " ++ show (unContractInstanceId ciID) ++ " succeeded."
    Just err -> "Contract instance " ++ show (unContractInstanceId ciID) ++ " failed with error: " ++ show err
  sub <- readMVar mvar
  R.removeSubscriber cardRc sub
handleInstanceChange _ _ _ = return ()

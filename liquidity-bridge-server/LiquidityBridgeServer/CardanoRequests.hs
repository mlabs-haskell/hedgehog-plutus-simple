{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module LiquidityBridgeServer.CardanoRequests (
  CardMessage (..),
  CardReceiver,
  CardSubscription (..),
  subscribeContractInstance,
  subscribeWallet,
) where

import Control.Concurrent (ThreadId, killThread)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import LiquidityBridgeServer.CustomJSON (customParseJSON, customToJSON)
import LiquidityBridgeServer.Requests (
  Receiver (..),
  ReceiverSubscription (..),
  addListener,
 )
import Network.WebSockets (sendTextData)
import Plutus.PAB.Events.Contract (ContractInstanceId)
import Wallet.Emulator.Wallet (Wallet (..))
import Prelude

{- | Cardano subscription containing the watcher thread id and the event listened to

 @since 0.1
-}
data CardSubscription = CardSubscription
  { csThreadId :: ThreadId
  , csSubscriptionTopic :: Either ContractInstanceId Wallet
  }
  deriving stock (Show, Eq)

{- | Data type to capture the response body of a message from the PAB simulator
 Replace with the real plutus data type if it exists

 @since 0.1
-}
data CardMessage (a :: Type) = CardMessage
  { cmContents :: a
  , cmTag :: Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON a => ToJSON (CardMessage a) where
  toJSON = customToJSON 2

instance FromJSON a => FromJSON (CardMessage a) where
  parseJSON = customParseJSON 2

{- | Wrapper for the CardMessage type, since we cannot subscribe on arbitrary requests for cardano, so we discard the request type var

 @since 0.1
-}
newtype CardSubReq a = CardSubReq (CardMessage (Either ContractInstanceId Wallet))

instance ReceiverSubscription CardSubscription where
  type ReceiverError CardSubscription = ()
  type ReceiverRequest CardSubscription = CardSubReq
  destroySubscriber = removeCardSubscriber
  createSubscriber = addCardSubscriber

type CardReceiver = Receiver CardSubscription

{- | Creates the request message to subscribe to a Wallet

 @since 0.1
-}
subscribeWallet :: Wallet -> CardSubReq ()
subscribeWallet w = CardSubReq $ CardMessage (Right w) "Subscribe"

{- | Creates the request message to subscribe to a contract instance

 @since 0.1
-}
subscribeContractInstance :: ContractInstanceId -> CardSubReq ()
subscribeContractInstance ciID = CardSubReq $ CardMessage (Left ciID) "Subscribe"

{- | Deletes the thread and sends the unsubscribe message

 @since 0.1
-}
removeCardSubscriber :: CardReceiver -> CardSubscription -> IO ()
removeCardSubscriber rc sub = do
  killThread sub.csThreadId
  sendCardRequest rc $ CardMessage sub.csSubscriptionTopic "Unsubscribe"

{- | Gets the event tag for a subscription based on the message sent
  This is hard coded in the Plutus repo, and non derivable

 @since 0.1
-}
getResponseName :: CardMessage (Either ContractInstanceId Wallet) -> Text
getResponseName (CardMessage (Left _) _) = "InstanceUpdate"
getResponseName (CardMessage (Right _) _) = "WalletFundsChange"

{- | Sends the subscription message and listens to replies with the correct tag

 @since 0.1
-}
addCardSubscriber ::
  forall (response :: Type) (request :: Type) (a :: Type).
  (FromJSON response) =>
  Receiver a ->
  CardSubReq request ->
  (response -> IO ()) ->
  IO (Either () CardSubscription)
addCardSubscriber rc (CardSubReq req) callback = do
  sendCardRequest rc req
  let tag = getResponseName req
  tId <- addListener @response rc (checkSubscriptionResponse @a tag) callback
  return $ Right $ CardSubscription tId req.cmContents

{- | Checks a message recode to the right type with the correct tag

 @since 0.1
-}
checkSubscriptionResponse ::
  forall (a :: Type) (response :: Type).
  FromJSON response =>
  Text ->
  ByteString ->
  Maybe (Either (ReceiverError a) response)
checkSubscriptionResponse tag str = do
  msg <- decode @(CardMessage response) str
  if msg.cmTag == tag
    then return $ Right msg.cmContents
    else Nothing

{- | Sends a cardano transaction, only used for subscribing and unsubscribing

 @since 0.1
-}
sendCardRequest ::
  forall (request :: Type) (a :: Type).
  ToJSON request =>
  Receiver a ->
  request ->
  IO ()
sendCardRequest rc req = sendTextData rc.rConnection $ encode req

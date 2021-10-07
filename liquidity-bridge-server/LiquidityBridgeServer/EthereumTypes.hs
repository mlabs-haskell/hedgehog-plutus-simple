{-# LANGUAGE DeriveAnyClass #-}

module LiquidityBridgeServer.EthereumTypes (
  module LiquidityBridgeServer.EthereumValues,
  EthError (..),
  EthLogEvent (..),
  EthLogFilterTopic (..),
  EthLogsFilter (..),
  EthLogsUnsubscribe (..),
  EthMessage (..),
  EthOutTransaction (..),
  EthRequest,
  EthRequestParams (..),
  EthResponse,
  EthResponseParams (..),
  EthSubscribedEvent,
  EthTransaction (..),
  EthTransactionReceipt (..),
  ethGetReceipt,
  ethLogsFilter,
  ethOutTransaction,
  getLogs,
  getTransactionByHash,
  newAccount,
  subscribeLogs,
  unsubscribe,
  web3version,
) where

import Data.Aeson (
  FromJSON,
  Object,
  Options (..),
  ToJSON,
  Value (..),
  genericToJSON,
  object,
  parseJSON,
  toJSON,
  withObject,
  (.!=),
  (.:!),
  (.:?),
  (.=),
 )
import Data.HashMap.Strict qualified as Map
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Vector (Vector, singleton)
import GHC.Generics (Generic)
import LiquidityBridgeServer.CustomJSON (customOptions, customParseJSON, customToJSON)
import LiquidityBridgeServer.EthereumValues (EthBool (..), EthNumber (..), EthString (..), padLeft)
import Prelude hiding (drop, take)

{- | General message structure for eth

 @since 0.1
-}
data EthMessage (a :: Type) = EthMessage
  { emContents :: a
  , emId :: Integer
  }
  deriving stock (Generic, Show, Eq)

{- | eth puts response specific tags in same level as general tags like id and jsonrpc
  This joins embedded message in the same level, but only if sub-structure jsonifies to an Object
  (Otherwise the sub-structure is omitted)

 @since 0.1
-}
instance ToJSON a => ToJSON (EthMessage a) where
  toJSON (EthMessage c i) =
    Object
      ( Map.fromList
          [ "id" .= i
          , "jsonrpc" .= ("2.0" :: Text)
          ]
          <> getObject (toJSON c)
      )
    where
      getObject :: Value -> Object
      getObject (Object cObject) = cObject
      getObject _ = mempty

{- | Similar to ToJSON, expands the flattened structure into something useful

 @since 0.1
-}
instance FromJSON a => FromJSON (EthMessage a) where
  parseJSON = withObject "EthMessage" $ \v ->
    EthMessage
      <$> (parseJSON @a $ Object v)
      <*> v .:? "id" .!= (-1)

{- | All requests take a method and params.
  The params are often a list, but can be type heterogeneous, so we leave it fully parameterized

 @since 0.1
-}
data EthRequestParams (a :: Type) = EthRequestParams
  { eReqMethod :: Text
  , eReqParams :: a
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON a => ToJSON (EthRequestParams a) where
  toJSON = customToJSON 4

instance FromJSON a => FromJSON (EthRequestParams a) where
  parseJSON = customParseJSON 4

{- | Request message wrapper

 @since 0.1
-}
type EthRequest a = EthMessage (EthRequestParams a)

{- | Creates a new account EthRequest from a password

 @since 0.1
-}
newAccount :: Text -> EthRequestParams (Vector Text)
newAccount = EthRequestParams "personal_newAccount" . singleton

{- | Data type for log filter topics

 @since 0.1
-}
data EthLogFilterTopic
  = EthLogFilterTopicAny
  | EthLogFilterTopicSingle Text
  | EthLogFilterTopicList (Vector Text)
  deriving (Show, Eq)

instance ToJSON EthLogFilterTopic where
  toJSON EthLogFilterTopicAny = Null
  toJSON (EthLogFilterTopicSingle x) = String x
  toJSON (EthLogFilterTopicList xs) = Array (String <$> xs)

{- | Data type for log filter

 @since 0.1
-}
data EthLogsFilter = EthLogsFilter
  { elfFromBlock :: Maybe EthNumber
  , elfToBlock :: Maybe EthNumber
  , elfAddress :: Maybe (Vector Text)
  , elfTopics :: Maybe (Vector EthLogFilterTopic)
  , elfLimit :: Maybe EthNumber
  }
  deriving stock (Generic, Show, Eq)

{- | Default parameters for a filter (empty / accepts all)

 @since 0.1
-}
ethLogsFilter :: EthLogsFilter
ethLogsFilter =
  EthLogsFilter
    { elfFromBlock = Nothing
    , elfToBlock = Nothing
    , elfAddress = Nothing
    , elfTopics = Nothing
    , elfLimit = Nothing
    }

{- | Builds subscribe params for the filter

 @since 0.1
-}
instance ToJSON EthLogsFilter where
  toJSON = genericToJSON ((customOptions 3) {omitNothingFields = True})

{- | Creates a general log subscribe request

 @since 0.1
-}
subscribeLogs :: EthLogsFilter -> EthRequestParams (String, EthLogsFilter)
subscribeLogs obj = EthRequestParams "eth_subscribe" ("logs", obj)

{- | Creates a get logs request from a filter

 @since 0.1
-}
getLogs :: EthLogsFilter -> EthRequestParams [EthLogsFilter]
getLogs obj = EthRequestParams "eth_getLogs" [obj]

{- | Unsubscription request params

 @since 0.1
-}
data EthLogsUnsubscribe = EthLogsUnsubscribe (Vector EthNumber)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

{- | Creates an unsubscribe request

 @since 0.1
-}
unsubscribe :: EthNumber -> EthRequestParams EthLogsUnsubscribe
unsubscribe = EthRequestParams "eth_unsubscribe" . EthLogsUnsubscribe . singleton

{- | Gets the web3 client version (simple request for testing)

 @since 0.1
-}
web3version :: EthRequestParams ()
web3version = EthRequestParams "web3_clientVersion" ()

{- | Gets a transaction by its hash as returned by subscribed event callbacks

 @since 0.1
-}
getTransactionByHash :: Text -> EthRequestParams (Vector Text)
getTransactionByHash = EthRequestParams "eth_getTransactionByHash" . singleton

{- | Data type for eth error responses

 @since 0.1
-}
data EthError = EthError
  { eeCode :: Integer
  , eeMessage :: Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EthError where
  toJSON = customToJSON 2

instance FromJSON EthError where
  parseJSON = customParseJSON 2

{- | General structure of eth response message params

 @since 0.1
-}
data EthResponseParams (a :: Type) = EthResponseParams
  { eResResult :: Either EthError a
  , eResSubscription :: Maybe EthNumber
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON a => ToJSON (EthResponseParams a) where
  toJSON obj =
    object $
      (either (("error" .=) . toJSON) (("result" .=) . toJSON) obj.eResResult) :
      maybe [] (pure . ("subscription" .=) . toJSON) obj.eResSubscription

instance FromJSON a => FromJSON (EthResponseParams a) where
  parseJSON = withObject "EthResponseParams" $ \v -> do
    mResult <- v .:! "result"
    mError <- v .:! "error"
    mSubscription <- v .:? "subscription"
    let result = maybe (Left $ fromJust mError) Right mResult
    return $ EthResponseParams result mSubscription

{- | Response message wrapper

 @since 0.1
-}
type EthResponse a = EthMessage (EthResponseParams a)

{- | Subscription event wrapper. Structured like an outgoing request but without an id.

 @since 0.1
-}
type EthSubscribedEvent a = EthMessage (EthRequestParams (EthResponseParams a))

{- | Data type for ethereum transactions sent via eth

 @since 0.1
-}
data EthOutTransaction = EthOutTransaction
  { eotFrom :: Text
  , eotTo :: Text
  , eotGas :: EthNumber
  , eotData :: Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EthOutTransaction where
  toJSON = customToJSON 3

instance FromJSON EthOutTransaction where
  parseJSON = customParseJSON 3

ethOutTransaction :: EthOutTransaction -> EthRequestParams [EthOutTransaction]
ethOutTransaction eot = EthRequestParams "eth_sendTransaction" [eot]

{- | Data type for ethereum transactions as returned by eth

 @since 0.1
-}
data EthTransaction = EthTransaction
  { etBlockHash :: Text
  , etBlockNumber :: EthNumber
  , etChainId :: EthNumber
  , etCreates :: Maybe Text
  , etFrom :: Text
  , etGas :: EthNumber
  , etGasPrice :: EthNumber
  , etHash :: Text
  , etInput :: EthString
  , etNonce :: EthNumber
  , etPublicKey :: Text
  , etTo :: Text
  , etTransactionIndex :: EthNumber
  , etValue :: EthNumber
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EthTransaction where
  toJSON = customToJSON 2

instance FromJSON EthTransaction where
  parseJSON = customParseJSON 2

{- | Data type for transaction receipts returned by the node

 @since 0.1
-}
data EthTransactionReceipt = EthTransactionReceipt
  { etrBlockHash :: Text
  , etrBlockNumber :: EthNumber
  , etrContractAddress :: Maybe Text
  , etrCumulativeGasUsed :: EthNumber
  , etrFrom :: Text
  , etrTo :: Text
  , etrGasUsed :: EthNumber
  , etrLogs :: [EthLogEvent Text]
  , etrLogsBloom :: Text
  , etrRoot :: Maybe Text
  , etrStatus :: EthBool
  , etrTransactionHash :: Text
  , etrTransactionIndex :: EthNumber
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EthTransactionReceipt where
  toJSON = customToJSON 3

instance FromJSON EthTransactionReceipt where
  parseJSON = customParseJSON 3

{- | Request for a receipt from the transaction hash

 @since 0.1
-}
ethGetReceipt :: Text -> EthRequestParams [Text]
ethGetReceipt hash = EthRequestParams "eth_getTransactionReceipt" [hash]

{- | Data type for log events

 @since 0.1
-}
data EthLogEvent (a :: Type) = EthLogEvent
  { eleLogIndex :: EthNumber
  , eleBlockNumber :: EthNumber
  , eleBlockHash :: Text
  , eleTransactionHash :: Text
  , eleTransactionIndex :: EthNumber
  , eleAddress :: Text
  , eleData :: a
  , eleTopics :: Vector Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON a => ToJSON (EthLogEvent a) where
  toJSON = customToJSON 3

instance FromJSON a => FromJSON (EthLogEvent a) where
  parseJSON = customParseJSON 3

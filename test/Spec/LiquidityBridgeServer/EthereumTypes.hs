module Spec.LiquidityBridgeServer.EthereumTypes (tests) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import Data.Vector (Vector, singleton)
import LiquidityBridgeServer.EthereumTypes (
  EthBool (..),
  EthError (..),
  EthLogEvent (..),
  EthLogFilterTopic (..),
  EthLogsFilter (..),
  EthLogsUnsubscribe (..),
  EthMessage (..),
  EthNumber (..),
  EthOutTransaction (..),
  EthRequestParams (..),
  EthResponseParams (..),
  EthTransaction (..),
  EthTransactionReceipt (..),
 )
import Spec.LiquidityBridgeServer.EthereumValues (getASCIIEthString)
import Spec.LiquidityBridgeServer.QuickCheckHelper (testJSON)
import Test.QuickCheck (ASCIIString (..), NonNegative (..))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude

{- | Tests JSON properties of EthereumTypes

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "EthereumTypes"
    [ testJSONEthLogEvent
    , testJSONEthError
    , testJSONEthResponseParams
    , testJSONEthRequestParams
    , testJSONEthLogsUnsubscribe
    , testJSONEthTransaction
    , testJSONEthLogFilterTopic
    , testJSONEthLogsFilter
    , testJSONEthMessage
    , testJSONEthOutTransaction
    , testJSONEthTransactionReceipt
    ]

{- | JSON Tests for EthLogEvent
  This structure is a little complicated, so we do more than default number of tests

 @since 0.1
-}
testJSONEthLogEvent :: TestTree
testJSONEthLogEvent =
  testJSON @(EthLogEvent ()) @(NonNegative Integer, Text, Vector Text)
    "EthLogEvent"
    500
    ( \(NonNegative num, str, strVec) ->
        encodingToLazyByteString . value . object $
          [ "logIndex" .= toJSON (EthNumber num)
          , "blockNumber" .= toJSON (EthNumber num)
          , "blockHash" .= toJSON str
          , "transactionHash" .= toJSON str
          , "transactionIndex" .= toJSON (EthNumber num)
          , "address" .= toJSON str
          , "data" .= toJSON ()
          , "topics" .= toJSON strVec
          ]
    )
    ( \(NonNegative num, str, strVec) ->
        EthLogEvent
          { eleLogIndex = EthNumber num
          , eleBlockNumber = EthNumber num
          , eleBlockHash = str
          , eleTransactionHash = str
          , eleTransactionIndex = EthNumber num
          , eleAddress = str
          , eleData = ()
          , eleTopics = strVec
          }
    )

testJSONEthError :: TestTree
testJSONEthError =
  testJSON @EthError @(Integer, Text)
    "EthError"
    100
    ( \(a, b) ->
        encodingToLazyByteString . value . object $
          [ "code" .= toJSON a
          , "message" .= toJSON b
          ]
    )
    (uncurry EthError)

removeNulls :: [(Text, Value)] -> [(Text, Value)]
removeNulls = filter $ (/= Null) . snd

testJSONEthResponseParams :: TestTree
testJSONEthResponseParams =
  testJSON @(EthResponseParams ()) @(Either (Integer, Text) (), Maybe (NonNegative Integer))
    "EthResponseParams"
    100
    ( \(a, b) ->
        encodingToLazyByteString . value . object $
          either (("error" .=) . toJSON . uncurry EthError) (("result" .=) . toJSON) a :
          removeNulls ["subscription" .= toJSON (EthNumber . getNonNegative <$> b)]
    )
    ( \(a, b) ->
        EthResponseParams
          { eResResult = mapLeft (uncurry EthError) a
          , eResSubscription = EthNumber . getNonNegative <$> b
          }
    )

testJSONEthRequestParams :: TestTree
testJSONEthRequestParams =
  testJSON @(EthRequestParams ()) @Text
    "EthRequestParams"
    100
    ( \x ->
        encodingToLazyByteString . value . object $
          [ "method" .= toJSON x
          , "params" .= toJSON ()
          ]
    )
    (`EthRequestParams` ())

testJSONEthLogsUnsubscribe :: TestTree
testJSONEthLogsUnsubscribe =
  testJSON @EthLogsUnsubscribe @(NonNegative Integer)
    "EthLogsUnsubscribe"
    100
    (encodingToLazyByteString . value . Array . singleton . toJSON . EthNumber . getNonNegative)
    (EthLogsUnsubscribe . singleton . EthNumber . getNonNegative)

{- | JSON Tests for EthTransaction
  This structure is a little complicated, so we do more than default number of tests

 @since 0.1
-}
testJSONEthTransaction :: TestTree
testJSONEthTransaction =
  testJSON @EthTransaction @(NonNegative Integer, Text, Maybe Text, ASCIIString)
    "EthTransaction"
    500
    ( \(NonNegative num, str, mStr, aStr) ->
        encodingToLazyByteString . value . object $
          [ "blockHash" .= toJSON str
          , "blockNumber" .= toJSON (EthNumber num)
          , "chainId" .= toJSON (EthNumber num)
          , "creates" .= toJSON mStr
          , "from" .= toJSON str
          , "gas" .= toJSON (EthNumber num)
          , "gasPrice" .= toJSON (EthNumber num)
          , "hash" .= toJSON str
          , "input" .= toJSON (getASCIIEthString aStr)
          , "nonce" .= toJSON (EthNumber num)
          , "publicKey" .= toJSON str
          , "to" .= toJSON str
          , "transactionIndex" .= toJSON (EthNumber num)
          , "value" .= toJSON (EthNumber num)
          ]
    )
    ( \(NonNegative num, str, mStr, aStr) ->
        EthTransaction
          { etBlockHash = str
          , etBlockNumber = EthNumber num
          , etChainId = EthNumber num
          , etCreates = mStr
          , etFrom = str
          , etGas = EthNumber num
          , etGasPrice = EthNumber num
          , etHash = str
          , etInput = getASCIIEthString aStr
          , etNonce = EthNumber num
          , etPublicKey = str
          , etTo = str
          , etTransactionIndex = EthNumber num
          , etValue = EthNumber num
          }
    )

testJSONEthLogFilterTopic :: TestTree
testJSONEthLogFilterTopic =
  testGroup
    "EthLogFilterTopic"
    [ testProperty "Null" $ encode EthLogFilterTopicAny == "null"
    , testProperty "Single" $ \x ->
        encode (EthLogFilterTopicSingle x)
          == (encodingToLazyByteString . value . String $ x)
    , testProperty "List" $ \x ->
        encode (EthLogFilterTopicList x)
          == (encodingToLazyByteString . value . Array . fmap String $ x)
    ]

testJSONEthLogsFilter :: TestTree
testJSONEthLogsFilter = testProperty "EthLogsFilter" testJSONEthLogsFilterProperty

testJSONEthLogsFilterProperty ::
  ( Maybe (NonNegative Integer)
  , Maybe (Vector Text)
  , Maybe (Vector (Maybe (Either Text (Vector Text))))
  ) ->
  Bool
testJSONEthLogsFilterProperty (mi, mvt, mve) = encode v == s
  where
    v :: EthLogsFilter
    v =
      EthLogsFilter
        { elfFromBlock = EthNumber . getNonNegative <$> mi
        , elfToBlock = EthNumber . getNonNegative <$> mi
        , elfAddress = mvt
        , elfTopics = fmap (fmap toEthTopic) mve
        , elfLimit = EthNumber . getNonNegative <$> mi
        }

    toEthTopic :: Maybe (Either Text (Vector Text)) -> EthLogFilterTopic
    toEthTopic Nothing = EthLogFilterTopicAny
    toEthTopic (Just (Left t)) = EthLogFilterTopicSingle t
    toEthTopic (Just (Right vt)) = EthLogFilterTopicList vt

    s :: ByteString
    s =
      encodingToLazyByteString . value . object . removeNulls $
        [ "fromBlock" .= toJSON (EthNumber . getNonNegative <$> mi)
        , "toBlock" .= toJSON (EthNumber . getNonNegative <$> mi)
        , "address" .= toJSON mvt
        , "topics" .= toJSON (fmap (fmap toValue) mve)
        , "limit" .= toJSON (EthNumber . getNonNegative <$> mi)
        ]

    toValue :: Maybe (Either Text (Vector Text)) -> Value
    toValue Nothing = Null
    toValue (Just (Left t)) = toJSON t
    toValue (Just (Right vt)) = toJSON vt

-- We're using error as it's simple and easy to encode. EthError is tested above.
testJSONEthMessage :: TestTree
testJSONEthMessage =
  testJSON @(EthMessage EthError) @(Text, Integer)
    "EthMessage"
    100
    ( \(t, i) ->
        encodingToLazyByteString . value . object $
          [ "id" .= toJSON i
          , "jsonrpc" .= toJSON ("2.0" :: Text)
          , "code" .= toJSON i
          , "message" .= toJSON t
          ]
    )
    (\(t, i) -> EthMessage (EthError i t) i)

testJSONEthOutTransaction :: TestTree
testJSONEthOutTransaction =
  testJSON @EthOutTransaction @(Text, NonNegative Integer)
    "EthOutTransaction"
    100
    ( \(str, NonNegative num) ->
        encodingToLazyByteString . value . object $
          [ "from" .= toJSON str
          , "to" .= toJSON str
          , "gas" .= toJSON (EthNumber num)
          , "data" .= toJSON str
          ]
    )
    ( \(str, NonNegative num) ->
        EthOutTransaction
          { eotFrom = str
          , eotTo = str
          , eotGas = EthNumber num
          , eotData = str
          }
    )

{- | JSON Tests for EthTransactionReceipt
  This structure is a little complicated, so we do more than default number of tests

 @since 0.1
-}
testJSONEthTransactionReceipt :: TestTree
testJSONEthTransactionReceipt =
  testJSON @EthTransactionReceipt @(NonNegative Integer, Text, Maybe Text, Bool)
    "EthTransactionReceipt"
    500
    ( \(NonNegative num, str, mStr, b) ->
        encodingToLazyByteString . value . object $
          [ "blockHash" .= toJSON str
          , "blockNumber" .= toJSON (EthNumber num)
          , "contractAddress" .= toJSON mStr
          , "cumulativeGasUsed" .= toJSON (EthNumber num)
          , "from" .= toJSON str
          , "to" .= toJSON str
          , "gasUsed" .= toJSON (EthNumber num)
          , "logs" .= toJSON ([] :: [EthLogEvent Text])
          , "logsBloom" .= toJSON str
          , "root" .= toJSON mStr
          , "status" .= toJSON (EthBool b)
          , "transactionHash" .= toJSON str
          , "transactionIndex" .= toJSON (EthNumber num)
          ]
    )
    ( \(NonNegative num, str, mStr, b) ->
        EthTransactionReceipt
          { etrBlockHash = str
          , etrBlockNumber = EthNumber num
          , etrContractAddress = mStr
          , etrCumulativeGasUsed = EthNumber num
          , etrFrom = str
          , etrTo = str
          , etrGasUsed = EthNumber num
          , etrLogs = []
          , etrLogsBloom = str
          , etrRoot = mStr
          , etrStatus = EthBool b
          , etrTransactionHash = str
          , etrTransactionIndex = EthNumber num
          }
    )

module Spec.LiquidityBridgeServer.CardanoRequests (tests) where

import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import Data.Text (Text)
import LiquidityBridgeServer.CardanoRequests (CardMessage (..))
import Spec.LiquidityBridgeServer.QuickCheckHelper (testJSON)
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Prelude

{- | Tests JSON properties of CardanoRequests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "CardanoRequests"
    [testJSONCardMessage]

testJSONCardMessage :: TestTree
testJSONCardMessage =
  testJSON @(CardMessage ()) @Text
    "CardMessage"
    100
    ( \s ->
        encodingToLazyByteString . value . object $
          [ "tag" .= toJSON s
          , "contents" .= toJSON ()
          ]
    )
    (CardMessage ())

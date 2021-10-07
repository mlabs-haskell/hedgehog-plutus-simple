module Spec.LiquidityBridgeServer.EthereumValues (getASCIIEthString, tests) where

import Data.Aeson (decode, encode)
import Data.Char (ord)
import Data.String (fromString)
import LiquidityBridgeServer.EthereumValues (EthBool (..), EthNumber (..), EthString (..))
import Numeric (showHex)
import Spec.LiquidityBridgeServer.QuickCheckHelper (testJSON)
import Test.QuickCheck (ASCIIString (..), NonNegative (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude

{- | Tests JSON properties of EthereumValues

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "EthereumValues"
    [ testJSONEthNumber
    , testJSONEthString
    , testJSONEthBool
    ]

{- | Converts an ASCII string to a EthString, removing any leading \NUL characters

 @since 0.1
-}
getASCIIEthString :: ASCIIString -> EthString
getASCIIEthString = EthString . dropWhile (== '\NUL') . getASCIIString

testJSONEthNumber :: TestTree
testJSONEthNumber =
  testJSON @EthNumber @(NonNegative Integer)
    "EthNumber"
    100
    (\(NonNegative x) -> "\"0x" <> fromString (showHex x "") <> "\"")
    (EthNumber . getNonNegative)

testJSONEthString :: TestTree
testJSONEthString =
  testJSON @EthString @ASCIIString
    "EthString"
    100
    (\(ASCIIString s) -> "\"0x" <> (fromString . flip concatMap (dropWhile (== '\NUL') s) $ padByte . flip showHex "" . ord) <> "\"")
    getASCIIEthString

padByte :: String -> String
padByte [c] = '0' : [c]
padByte s = s

testJSONEthBool :: TestTree
testJSONEthBool =
  testGroup
    "EthBool"
    [ testProperty "Encode True" $ encode (EthBool True) == "\"0x1\""
    , testProperty "Encode False" $ encode (EthBool False) == "\"0x0\""
    , testProperty "Decode True" $ decode @EthBool "\"0x1\"" == Just (EthBool True)
    , testProperty "Decode False" $ decode @EthBool "\"0x0\"" == Just (EthBool False)
    ]

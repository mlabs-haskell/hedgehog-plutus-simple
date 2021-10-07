module Spec.LiquidityBridgeServer.QuickCheckHelper where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Prelude

{- | Tests identity for an element before and after encode and decode

 @since 0.1
-}
reflexiveTest ::
  forall (a :: Type).
  (FromJSON a, ToJSON a, Eq a) =>
  a ->
  Bool
reflexiveTest a = (decode . encode $ a) == Just a

testJSON ::
  forall (obj :: Type) (p :: Type).
  ( Arbitrary p
  , Show p
  , ToJSON obj
  , FromJSON obj
  , Eq obj
  ) =>
  String ->
  Int ->
  (p -> ByteString) ->
  (p -> obj) ->
  TestTree
testJSON name count fJSON fObj =
  testGroup
    name
    $ fmap
      setTestCount
      [ testProperty @(p -> Bool) "Reflexive JSON" $ \v -> reflexiveTest $ fObj v
      , testProperty @(p -> Bool) "Encode JSON" $ \v -> encode (fObj v) == fJSON v
      , testProperty @(p -> Bool) "Decode JSON" $ \v -> decode (fJSON v) == Just (fObj v)
      ]
  where
    setTestCount :: TestTree -> TestTree
    setTestCount = localOption $ QuickCheckTests count

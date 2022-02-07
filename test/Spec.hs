module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Prelude (IO)

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "LiquidityBridge"
    []

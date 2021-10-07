module Main (main) where

import Spec.LiquidityBridge.PaymentConfirmation qualified as PaymentConfirmation
import Spec.LiquidityBridgeServer.CardanoRequests qualified as CardanoRequests
import Spec.LiquidityBridgeServer.EthereumTypes qualified as EthereumTypes
import Spec.LiquidityBridgeServer.EthereumValues qualified as EthereumValues
import Test.Tasty
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
    [ testGroup
        "LiquidityBridge"
        [ PaymentConfirmation.tests
        ]
    , testGroup
        "LiquidityBridgeServer"
        [ EthereumValues.tests
        , EthereumTypes.tests
        , CardanoRequests.tests
        ]
    ]

module LiquidityBridge (endpoints) where

import LiquidityBridge.PaymentConfirmation (paymentConfirmation, paymentConfirmationEth)
import LiquidityBridge.Schema (LiquidityBridgeSchema)
import Plutus.Contract (Contract, ContractError, endpoint, selectList)

{- | Playground API

 @since 0.1
-}
endpoints :: Contract () LiquidityBridgeSchema ContractError ()
endpoints =
  selectList
    [ endpoint @"PaymentConfirmation" paymentConfirmation
    , endpoint @"PaymentConfirmationEth" paymentConfirmationEth
    ]

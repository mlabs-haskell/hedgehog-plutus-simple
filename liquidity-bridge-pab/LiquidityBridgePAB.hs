{-# LANGUAGE DeriveAnyClass #-}

module LiquidityBridgePAB (main) where

import Cardano.Prelude (liftIO, threadDelay, (==))
import Control.Monad (when)
import Control.Monad.Freer (Eff, interpret)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (getLine)
import Data.Default (def)
import Data.Kind (Type)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import LiquidityBridge.PaymentConfirmation (paymentConfirmationEthFromPubKey, paymentConfirmationFromPubKey)
import LiquidityBridge.Schema (LiquidityBridgeSchema, PCEParams (..), PCParams (..))
import LiquidityBridgePAB.Logging
import Playground.Types (FunctionSchema)
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.Effects.Contract.Builtin (
  Builtin,
  BuiltinHandler (contractHandler),
  HasDefinitions (getContract, getDefinitions, getSchema),
  SomeBuiltin (..),
  endpointsToSchemas,
  handleBuiltin,
 )
import Plutus.PAB.PrettyLogger
import Plutus.PAB.Simulator (SimulatorContractHandler, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as Server
import PlutusTx.Prelude hiding ((==))
import Schema (FormSchema)
import Wallet.Emulator.Wallet (Wallet (..))
import Prelude (IO, Show)

instance HasDefinitions LiquidityBridgeContracts where
  getDefinitions :: [LiquidityBridgeContracts]
  getDefinitions = []

  getSchema :: LiquidityBridgeContracts -> [FunctionSchema FormSchema]
  getSchema = \case
    PaymentConfirmation _ -> endpointsToSchemas @LiquidityBridgeSchema
    PaymentConfirmationEth _ -> endpointsToSchemas @LiquidityBridgeSchema

  getContract :: (LiquidityBridgeContracts -> SomeBuiltin)
  getContract = \case
    PaymentConfirmation params -> SomeBuiltin $ paymentConfirmationFromPubKey (walletPkh serverWallet) params
    PaymentConfirmationEth params -> SomeBuiltin $ paymentConfirmationEthFromPubKey (walletPkh serverWallet) params

serverWallet :: Wallet
serverWallet = Wallet 1

clientWallet1 :: Wallet
clientWallet1 = Wallet 2

wallets :: Wallets
wallets =
  mkWallets
    [ (getWallet serverWallet, "Server")
    , (getWallet clientWallet1, "Client 1")
    ]

main :: IO ()
main = clearVoid $
  Simulator.runSimulationWith handlers $ do
    logAction "Starting Liquidity PAB Server."
    shutdown <- Server.startServerDebug
    logNewLine

    logAsciiLogo (Standard Blue) cardStarterLogo
    logAsciiLogo (Vibrant Red) mlabsLogo

    -- A moment to admire the pretty logos and connect websockets :)
    liftIO $ threadDelay 500_000

    logAction "Initialised wallets:"
    logWalletPubKeyHashes wallets
    logWalletBalances wallets

    logAction "Finished, press enter to stop server"

    let loop :: forall (t :: Type). Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
        loop = do
          text <- liftIO getLine
          when (text == "balance") $ do
            logWalletBalances wallets
            loop
    loop

    shutdown

handlers :: SimulatorEffectHandlers (Builtin LiquidityBridgeContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin LiquidityBridgeContracts)
    def
    def
    -- We *have* to specify the type here, I don't know why, but it took me hours to work out.
    (interpret (contractHandler handleBuiltin) :: SimulatorContractHandler (Builtin LiquidityBridgeContracts))

data LiquidityBridgeContracts
  = PaymentConfirmation PCParams
  | PaymentConfirmationEth PCEParams
  deriving stock
    ( -- | @since 0.1
      Show
    , -- | @since 0.1
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1
      FromJSON
    , -- | @since 0.1
      ToJSON
    )

instance Pretty LiquidityBridgeContracts where
  pretty = viaShow

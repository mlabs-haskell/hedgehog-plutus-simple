module LiquidityBridgePAB.Logging (
  cardStarterLogo,
  clearVoid,
  logAction,
  logAddressBalance,
  logAsciiLogo,
  logBalance,
  logWalletBalance,
  logWalletBalances,
  logWalletBalanceUnnamed,
  logWalletPubKeyHashes,
  mlabsLogo,
  mkWallets,
  walletPkh,
  Wallets (..),
) where

import Cardano.Prelude (MonadIO)
import Control.Monad (forM_, void)
import Control.Monad.Freer (Eff)
import Data.ByteString.Char8 qualified as Char8
import Data.Kind (Type)
import Ledger (Address, PubKeyHash, pubKeyHash)
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.PrettyLogger
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import Wallet.Emulator.Wallet qualified as Wallet
import Prelude (IO, Show, String, show)
import Prelude qualified

newtype Wallets = Wallets {getWallets :: Map.Map Integer String}
  deriving newtype (Semigroup, Monoid, Show)

mkWallets :: [(Integer, String)] -> Wallets
mkWallets = Wallets . Map.fromList

{- | Clears the screen then runs an IO computation, voiding the result

 @since 0.1
-}
clearVoid :: forall (a :: Type). IO a -> IO ()
clearVoid c = void $ clearScreen >> setCursorPosition 0 0 >> c

{- | Logs an action in Green

 @since 0.1
-}
logAction :: forall (m :: Type -> Type). MonadIO m => String -> m ()
logAction str = logPrettyColorBold (Vibrant Green) (withNewLines str)

{- | Get a wallets PubKeyHash

 @since 0.1
-}
walletPkh :: Wallet.Wallet -> PubKeyHash
walletPkh = pubKeyHash . Wallet.walletPubKey

{- | Logs all wallet pub key hashes

 @since 0.1
-}
logWalletPubKeyHashes ::
  forall (t :: Type).
  Wallets ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logWalletPubKeyHashes wallets = do
  logPretty "PubKeyHashes:"
  logNewLine
  forM_ (Map.toList wallets.getWallets) $ \(i, name) -> do
    logPretty $ name ++ ": " ++ show (walletPkh $ Wallet.Wallet i)
    logNewLine

{- | Width of the currency name column in logBalance functions

 @since 0.1
-}
logBalanceWidth :: Prelude.Int
logBalanceWidth = 20

{- | Logs balances for all wallets

 @since 0.1
-}
logWalletBalances ::
  forall (t :: Type).
  Wallets ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logWalletBalances wallets = mapM_ (logWalletBalance wallets . Wallet.Wallet . fst) $ Map.toList wallets.getWallets

{- | Logs balance for a wallet, using real name from Wallets

 @since 0.1
-}
logWalletBalance ::
  forall (t :: Type).
  Wallets ->
  Wallet.Wallet ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logWalletBalance wallets w = do
  let s = fromMaybe (show w) $ Map.lookup (Wallet.getWallet w) wallets.getWallets
  logAddressBalance s $ Wallet.walletAddress w

{- | Logs balance for an address, using a given name

 @since 0.1
-}
logAddressBalance ::
  forall (t :: Type).
  String ->
  Address ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logAddressBalance s addr = logBalance s =<< Simulator.valueAt addr

{- | Logs balance for a wallet, without a formal name

 @since 0.1
-}
logWalletBalanceUnnamed ::
  forall (t :: Type).
  Wallet.Wallet ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logWalletBalanceUnnamed = logWalletBalance mempty

{- | Logs a balance, listing all token names and values

 @since 0.1
-}
logBalance ::
  forall (m :: Type -> Type).
  MonadIO m =>
  String ->
  Value.Value ->
  m ()
logBalance walletLabel val = do
  logNewLine
  logPrettyBgColor 40 (Vibrant Cyan) (Standard Black) (padRight ' ' logBalanceWidth walletLabel ++ "BALANCE")
  logNewLine
  logPrettyColor (Vibrant Cyan) (formatValue val)
  logNewLine

{- | Formats a balance into printable table

 @since 0.1
-}
formatValue :: Value.Value -> String
formatValue (Value.Value m) = concat $ prettyShow <$> Map.toList m
  where
    prettyShow :: (Value.CurrencySymbol, Map.Map Value.TokenName Integer) -> String
    prettyShow (_, v) = concat $ formatTokenValue <$> Map.toList v

    formatTokenValue :: (Value.TokenName, Integer) -> String
    formatTokenValue (_, 0) = ""
    formatTokenValue (name, value) =
      case name of
        "" -> padRight ' ' logBalanceWidth "Ada" ++ show value ++ "\n"
        (Value.TokenName n) -> padRight ' ' logBalanceWidth (Char8.unpack $ Builtins.fromBuiltin n) ++ show value ++ "\n"

{- | Logs a logo with a given colour

 @since 0.1
-}
logAsciiLogo ::
  forall (m :: Type -> Type).
  MonadIO m =>
  LogColor ->
  String ->
  m ()
logAsciiLogo logColor logo = do
  logNewLine
  logPrettyBgColor 40 logColor (Standard Black) logo
  logNewLine

{- | Our "logo"

 @since 0.1
-}
mlabsLogo :: String
mlabsLogo =
  Prelude.unlines
    [ "                                                 "
    , " ███╗   ███╗    ██╗      █████╗ ██████╗ ███████╗ "
    , " ████╗ ████║    ██║     ██╔══██╗██╔══██╗██╔════╝ "
    , " ██╔████╔██║    ██║     ███████║██████╔╝███████╗ "
    , " ██║╚██╔╝██║    ██║     ██╔══██║██╔══██╗╚════██║ "
    , " ██║ ╚═╝ ██║    ███████╗██║  ██║██████╔╝███████║ "
    , " ╚═╝     ╚═╝    ╚══════╝╚═╝  ╚═╝╚═════╝ ╚══════╝ "
    , "                                                 "
    ]

{- | CardStarter's "logo"

 @since 0.1
-}
cardStarterLogo :: String
cardStarterLogo =
  Prelude.unlines
    [ "                                                                                                               "
    , "  $$$$$$\\                            $$\\  $$$$$$\\    $$\\                          $$\\                          "
    , " $$  __$$\\                           $$ |$$  __$$\\   $$ |                         $$ |                         "
    , " $$ /  \\__| $$$$$$\\   $$$$$$\\   $$$$$$$ |$$ /  \\__|$$$$$$\\    $$$$$$\\   $$$$$$\\ $$$$$$\\    $$$$$$\\   $$$$$$\\   "
    , " $$ |       \\____$$\\ $$  __$$\\ $$  __$$ |\\$$$$$$\\  \\_$$  _|   \\____$$\\ $$  __$$\\\\_$$  _|  $$  __$$\\ $$  __$$\\  "
    , " $$ |       $$$$$$$ |$$ |  \\__|$$ /  $$ | \\____$$\\   $$ |     $$$$$$$ |$$ |  \\__| $$ |    $$$$$$$$ |$$ |  \\__| "
    , " $$ |  $$\\ $$  __$$ |$$ |      $$ |  $$ |$$\\   $$ |  $$ |$$\\ $$  __$$ |$$ |       $$ |$$\\ $$   ____|$$ |       "
    , " \\$$$$$$  |\\$$$$$$$ |$$ |      \\$$$$$$$ |\\$$$$$$  |  \\$$$$  |\\$$$$$$$ |$$ |       \\$$$$  |\\$$$$$$$\\ $$ |       "
    , "  \\______/  \\_______|\\__|       \\_______| \\______/    \\____/  \\_______|\\__|        \\____/  \\_______|\\__|       "
    , "                                                                                                               "
    ]

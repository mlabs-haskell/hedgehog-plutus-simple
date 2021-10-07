{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module LiquidityBridgeServer.Config (
  Config (..),
  Endpoint (..),
  getConfig,
) where

import Config
import Config.Schema
import Control.Exception (Exception, IOException, catch, displayException)
import Crypto.Hash (Keccak_256 (..), hashWith)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List.Extra (firstJust)
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text, drop, length, pack, take, unpack)
import Ledger (PubKeyHash)
import Numeric (readHex)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (replaceFileName)
import System.IO.Strict (readFile)
import Prelude hiding (drop, length, readFile, take)

{- | Config type for all server configuration

 @since 0.1
-}
data Config = Config
  { cardanoEndpoint :: Endpoint
  , ethereumEndpoint :: Endpoint
  , ethContractAddresses :: [String]
  , cardanoDexMap :: Map String PubKeyHash
  , ethCentralWalletAddress :: String
  , ethBlockMarkerPrefix :: String
  , cardanoDexMapChanged :: Bool
  , manualDexUpdate :: Bool
  }
  deriving (Show)

{- | Datatype for a webservice

 @since 0.1
-}
data Endpoint = Endpoint
  { url :: String
  , port :: Int
  , path :: String
  }
  deriving (Show)

{- | Top level specification for config file

 @since 0.1
-}
configSpec :: ValueSpec Config
configSpec = sectionsSpec "top-level configuration" $ do
  cardanoEndpoint <- reqSection' "cardano-endpoint" endpointSpec "Cardano endpoint"
  ethereumEndpoint <- reqSection' "ethereum-endpoint" endpointSpec "Ethereum endpoint"
  ethContractAddresses <- (unpack <$>) <$> reqSection' "eth-contracts" (oneOrList addressSpec) "Ethereum contract address"
  cardanoDexMap <- fromList <$> reqSection' "cardano-dexes" (oneOrList dexMapSpec) "Cardano Dex address map"
  ethCentralWalletAddress <- unpack <$> reqSection' "eth-wallet" addressSpec "Ethereum wallet address"
  ethBlockMarkerPrefix <- fromMaybe "./.ethmarker" <$> optSection' "block-file" stringSpec "Block marker file prefix"
  return Config {cardanoDexMapChanged = False, manualDexUpdate = False, ..}

{- | Webservice specification

 @since 0.1
-}
endpointSpec :: ValueSpec Endpoint
endpointSpec = sectionsSpec "" $ do
  url <- reqSection' "url" stringSpec "URL"
  port <- reqSection "port" "PORT"
  path <- fromMaybe "/" <$> optSection' "path" stringSpec "PATH"
  return Endpoint {..}

{- | Ethereum address specification

 @since 0.1
-}
addressSpec :: ValueSpec Text
addressSpec = customSpec "" textSpec $ \str ->
  if valid str then Right str else Left "Invalid address format"
  where
    valid :: Text -> Bool
    valid addr =
      (take 2 addr == "0x")
        && (length addr == 42)
        && case readHex @Integer $ unpack $ drop 2 addr of
          [(_, "")] -> True
          _ -> False

{- | Cardano dex name and address pair

 @since 0.1
-}
dexMapSpec :: ValueSpec (String, PubKeyHash)
dexMapSpec = sectionsSpec "" $ do
  name <- reqSection' "name" stringSpec "Dex Name"
  address <- fromString <$> reqSection' "address" stringSpec "Dex Cardano Address"
  return (name, address)

{- | Attempt to parse from string to config, returning error and usage

 @since 0.1
-}
parseConfig :: Text -> Either String Config
parseConfig s = either failed (either failed Right . loadValue configSpec) $ parse s
  where
    failed :: forall (e :: Type). Exception e => e -> Either String Config
    failed e = Left $ displayException e ++ "\n" ++ show (generateDocs configSpec)

{- | Attempt to read a file without erroring

 @since 0.1
-}
safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile p = catch @IOException (Just <$> readFile p) $ const $ pure Nothing

{- | Gets the hash of a configuration for change checking

 @since 0.1
-}
getDexMapHash :: Config -> String
getDexMapHash = show . hashWith Keccak_256 . fromString @ByteString . show . cardanoDexMap

{- | Gets the path for the config hash in the same location as the config

 @since 0.1
-}
getDexMapHashFilePath :: FilePath -> FilePath
getDexMapHashFilePath p = replaceFileName p ".dexmaphash"

{- | Updates changed on a config to be correct

 @since 0.1
-}
configChanged :: String -> Config -> Config
configChanged hash c = c {cardanoDexMapChanged = hash /= getDexMapHash c}

{- | Hashes and saves a config

 @since 0.1
-}
saveConfigHash :: FilePath -> Config -> IO ()
saveConfigHash path c =
  writeFile path $ getDexMapHash c {cardanoDexMapChanged = False}

{- | Gets the config given a config path along with if the config changed from the cached hash

 @since 0.1
-}
getConfigFromPath :: FilePath -> IO Config
getConfigFromPath p = do
  let hashPath = getDexMapHashFilePath p
  fileData <- safeReadFile p
  hash' <- safeReadFile hashPath
  let hash = fromMaybe "" hash'

  config <-
    maybe
      (exitFailStr "Couldn't find config file, default path: ./config.conf")
      (either exitFailStr (return . configChanged hash) . parseConfig . pack)
      fileData

  saveConfigHash hashPath config
  return config
  where
    exitFailStr :: forall (a :: Type). String -> IO a
    exitFailStr str = putStrLn str >> exitFailure

data Flag = UpdateFlag | ConfigFlag String deriving stock (Eq)

options :: [OptDescr Flag]
options =
  [ Option ['u'] ["update"] (NoArg UpdateFlag) "Force an update to all ethereum contracts"
  , Option ['c'] ["config"] (ReqArg ConfigFlag "FILE") "Config file path, default ./config.conf"
  ]

handleArgs :: [String] -> IO (Bool, String)
handleArgs argv =
  case getOpt Permute options argv of
    (fs, [], []) -> return $ handleFlags fs
    (_, _, errs) -> usageError errs

handleFlags :: [Flag] -> (Bool, String)
handleFlags fs = (UpdateFlag `elem` fs, fromMaybe "./config.conf" $ firstJust fromUpdateFlag fs)

fromUpdateFlag :: Flag -> Maybe String
fromUpdateFlag (ConfigFlag s) = Just s
fromUpdateFlag _ = Nothing

usageError :: [String] -> IO a
usageError errs = ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "\nUsage: liquidity-bridge-server [OPTION...]"

{- | Gets a config from the first commandline argument, or defaulting to "./config.conf"

 @since 0.1
-}
getConfig :: IO Config
getConfig = do
  (shouldUpdate, confPath) <- getArgs >>= handleArgs
  conf <- getConfigFromPath confPath
  return $ conf {manualDexUpdate = shouldUpdate}

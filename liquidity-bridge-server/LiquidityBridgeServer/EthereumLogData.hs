{-# LANGUAGE RecordWildCards #-}

module LiquidityBridgeServer.EthereumLogData (
  DepositEventParams,
  DepositEthEventParams,
) where

import Cardano.Prelude (atMay, uncons)
import Data.Aeson (FromJSON, Value (..), decode, parseJSON)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Kind (Type)
import Data.List.Split (chunksOf)
import Data.String (fromString)
import Data.Text (Text, drop, pack, take, unpack)
import Ledger (PubKeyHash (..))
import LiquidityBridgeServer.EthereumTypes qualified as ET
import Prelude hiding (drop, lookup, map, take)
import Prelude qualified

{- | Decode a hex number using the EthNumber decoding

 @since 0.1
-}
decodeNumber :: Text -> Maybe Integer
decodeNumber n = fmap ET.getEthNumber $ decode @ET.EthNumber $ fromString $ unpack $ "\"0x" <> n <> "\""

{- | Decode a hex string using the EthString decoding

 @since 0.1
-}
decodeString :: Text -> Maybe String
decodeString str = fmap ET.getEthString $ decode @ET.EthString $ fromString $ unpack $ "\"0x" <> str <> "\""

{- | Given the data list and an argument index, finds the string pointed to

 @since 0.1
-}
getDataText :: Int -> [Text] -> Maybe Text
getDataText chunkIndex chunks = do
  chunkStr <- chunks `atMay` chunkIndex
  dataPos <- fromInteger <$> decodeNumber chunkStr
  let chunkPos = dataPos `div` 32
      chunks' = Prelude.drop chunkPos chunks
  (dataLenStr, rest) <- uncons chunks'
  dataLen <- fromInteger <$> decodeNumber dataLenStr
  let strHex = take (dataLen * 2) $ mconcat rest
  str <- decodeString strHex

  if length str == dataLen
    then return $ pack str
    else Nothing

-- | Maybe to Parser that throws an error
fromMaybeErr :: forall (a :: Type). String -> Maybe a -> Parser a
fromMaybeErr errMsg = maybe (fail $ "Couldn't read " ++ errMsg) return

-- | Splits input hex string into the 32 byte chunks they are encoded as
segmentInput :: Value -> String -> Parser [Text]
segmentInput (String x) _ = return $ fmap pack $ chunksOf 64 $ unpack $ drop 2 x
segmentInput v s = typeMismatch s v

-- | Reads a 32 byte text
readText :: [Text] -> Int -> Parser Text
readText xs i = fromMaybeErr "Text" $ fmap pack $ decodeString =<< xs `atMay` i

-- | Reads an arbitrary length text
readMemoryText :: [Text] -> Int -> Parser Text
readMemoryText xs i = fromMaybeErr "Memory Text" $ getDataText i xs

-- | Reads a 20 byte ethereum address
readEthAddress :: [Text] -> Int -> Parser Text
readEthAddress xs i = fmap (("0x" <>) . drop 24) $ fromMaybeErr "Eth Address" $ xs `atMay` i

-- | Reads a 28 byte cardano pub key hash
readCardAddress :: [Text] -> Int -> Parser PubKeyHash
readCardAddress xs i = fmap (fromString . unpack . take 56) $ fromMaybeErr "Cardano Address" $ xs `atMay` i

-- | Reads an unsigned 32 byte integer
readInteger :: [Text] -> Int -> Parser Integer
readInteger xs i = fromMaybeErr "Integer" $ decodeNumber =<< xs `atMay` i

{- | Data type for the deposit event params

 @since 0.1
-}
data DepositEventParams = DepositEventParams
  { dpSender :: Text
  , dpCardanoAddress :: PubKeyHash
  , dpDexName :: Text
  , dpToken0Amount :: Integer
  , dpToken1Amount :: Integer
  , dpToken0Address :: Text
  , dpToken1Address :: Text
  , dpToken0Name :: Text
  , dpToken1Name :: Text
  }
  deriving stock (Show)

instance FromJSON DepositEventParams where
  parseJSON v = do
    xs <- segmentInput v "DepositEventParams"
    dpSender <- readEthAddress xs 0
    dpCardanoAddress <- readCardAddress xs 1
    dpDexName <- readText xs 2
    dpToken0Name <- readMemoryText xs 3
    dpToken1Name <- readMemoryText xs 4
    dpToken0Address <- readEthAddress xs 5
    dpToken1Address <- readEthAddress xs 6
    dpToken0Amount <- readInteger xs 7
    dpToken1Amount <- readInteger xs 8
    return DepositEventParams {..}

{- | Data type for the deposit weth event params

 @since 0.1
-}
data DepositEthEventParams = DepositEthEventParams
  { depSender :: Text
  , depCardanoAddress :: PubKeyHash
  , depAmount :: Integer
  }
  deriving stock (Show)

instance FromJSON DepositEthEventParams where
  parseJSON v = do
    xs <- segmentInput v "DepositEthEventParams"
    depSender <- readEthAddress xs 0
    depCardanoAddress <- readCardAddress xs 1
    depAmount <- readInteger xs 2
    return DepositEthEventParams {..}

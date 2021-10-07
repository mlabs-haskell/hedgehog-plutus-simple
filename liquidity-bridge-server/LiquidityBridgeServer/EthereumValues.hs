module LiquidityBridgeServer.EthereumValues (
  EthBool (..),
  EthNumber (..),
  EthString (..),
  padLeft,
) where

import Control.Monad (forM)
import Data.Aeson (
  FromJSON,
  ToJSON,
  Value (..),
  parseJSON,
  toJSON,
 )
import Data.Aeson.Types (typeMismatch)
import Data.Char (chr, ord)
import Data.List.Split (chunksOf)
import Data.String (IsString)
import Data.Text (drop, length, pack, take, unpack)
import GHC.Generics (Generic)
import Numeric (readHex, showHex)
import Prelude hiding (drop, take)

-- We use Strings over Text here as the hex functions work only on Strings

{- | Pads a string on the left to a minimum string length

 @since 0.1
-}
padLeft :: Int -> Char -> String -> String
padLeft i padChr str = replicate (i - Prelude.length str) padChr ++ str

{- | More friendly representation for eth numeric values

 @since 0.1
-}
newtype EthNumber = EthNumber
  {getEthNumber :: Integer}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Num)

instance ToJSON EthNumber where
  toJSON (EthNumber i) = String $ "0x" <> pack (showHex i "")

instance FromJSON EthNumber where
  parseJSON (String x)
    | take 2 x /= "0x" = fail "Invalid eth number"
    | otherwise = case readHex $ unpack $ drop 2 x of
      [(v, "")] -> return $ EthNumber v
      _ -> fail "Invalid eth number"
  parseJSON v = typeMismatch "EthNumber" v

{- | More friendly representation for eth byte streams

 @since 0.1
-}
newtype EthString = EthString
  {getEthString :: String}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (IsString)

instance ToJSON EthString where
  toJSON (EthString s) =
    String $ "0x" <> (pack . flip concatMap s $ padLeft 2 '0' . flip showHex "" . ord)

instance FromJSON EthString where
  parseJSON (String str)
    | odd $ Data.Text.length str = fail "Invalid eth string"
    | otherwise = case chunksOf 2 $ unpack str of
      ("0x" : xs) -> do
        xs' <- forM xs $ \c -> case readHex c of
          [(x, "")] -> return x
          _ -> fail "Invalid eth string"
        return $ EthString $ dropWhile (== '\NUL') $ fmap chr xs'
      _ -> fail "Invalid eth string"
  parseJSON v = typeMismatch "EthString" v

newtype EthBool = EthBool
  {getEthBool :: Bool}
  deriving stock (Eq, Show, Ord, Generic)

instance ToJSON EthBool where
  toJSON (EthBool True) = String "0x1"
  toJSON (EthBool False) = String "0x0"

instance FromJSON EthBool where
  parseJSON (String "0x1") = return $ EthBool True
  parseJSON (String "0x0") = return $ EthBool False
  parseJSON v = typeMismatch "EthBool" v

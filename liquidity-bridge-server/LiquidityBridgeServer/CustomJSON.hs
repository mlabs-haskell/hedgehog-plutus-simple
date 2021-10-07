module LiquidityBridgeServer.CustomJSON where

import Data.Aeson (
  GFromJSON,
  GToJSON',
  Options (..),
  Value (..),
  Zero,
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Aeson.Casing (camelCase)
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import GHC.Generics (Generic, Rep)
import Prelude

{- | Options for customToJSON and customParseJSON

 @since 0.1
-}
customOptions :: Int -> Options
customOptions i = defaultOptions {fieldLabelModifier = camelCase . drop i}

{- | Removes the first i characters from field names, and camelcases

 @since 0.1
-}
customToJSON ::
  forall (a :: Type).
  ( Generic a
  , GToJSON' Value Zero (Rep a)
  ) =>
  Int ->
  a ->
  Value
customToJSON = genericToJSON . customOptions

{- | Removes the first i characters from field names, and camelcases

 @since 0.1
-}
customParseJSON ::
  forall (a :: Type).
  ( Generic a
  , GFromJSON Zero (Rep a)
  ) =>
  Int ->
  Value ->
  Parser a
customParseJSON = genericParseJSON . customOptions

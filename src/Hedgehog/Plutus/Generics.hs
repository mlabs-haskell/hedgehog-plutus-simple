module Hedgehog.Plutus.Generics (
  Simple (Simple),
  Generically (Generically),
) where

import Data.Kind (Type)

type Simple :: Type -> Type
newtype Simple a = Simple a

type Generically :: Type -> Type
newtype Generically a = Generically a

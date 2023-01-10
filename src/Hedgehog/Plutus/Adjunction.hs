{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- Needed for Prelude import
{-# OPTIONS_GHC -Wwarn=missing-import-lists #-}

module Hedgehog.Plutus.Adjunction (
  Adjunction (Adjunction, left, right),
  adjunctionTest,
) where

import Prelude hiding (id, (.))

import Data.Kind (Type)

import Control.Category (Category (id, (.)))

import Hedgehog ((===))
import Hedgehog qualified

data Adjunction a b = Adjunction
  { left :: !(a -> b)
  , right :: !(b -> a)
  }

instance Category Adjunction where
  Adjunction ll lr . Adjunction rl rr =
    Adjunction
      { left = ll . rl
      , right = rr . lr
      }

  id =
    Adjunction
      { left = id
      , right = id
      }

-- | Verify the adjunction of a layer.
adjunctionTest ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Hedgehog.MonadTest m, Eq a, Show a) =>
  Adjunction a b ->
  a ->
  m ()
adjunctionTest (Adjunction left right) a = a === right (left a)

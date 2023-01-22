{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- Needed for Prelude import
{-# OPTIONS_GHC -Wwarn=missing-import-lists #-}

module Hedgehog.Plutus.Adjunction (
  Adjunction (Adjunction, lower, raise),
  adjunctionTest,
) where

import Prelude hiding (id, (.))

import Data.Kind (Type)

import Control.Category (Category (id, (.)))

import Hedgehog ((===))
import Hedgehog qualified

data Adjunction a b = Adjunction
  { lower :: !(a -> b)
  , raise :: !(b -> a)
  }

instance Category Adjunction where
  Adjunction ll lr . Adjunction rl rr =
    Adjunction
      { lower = ll . rl
      , raise = rr . lr
      }

  id =
    Adjunction
      { lower = id
      , raise = id
      }

-- | Verify the adjunction of a layer.
adjunctionTest ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Hedgehog.MonadTest m, Eq a, Show a) =>
  Adjunction a b ->
  a ->
  m ()
adjunctionTest (Adjunction lower raise) a = a === raise (lower a)

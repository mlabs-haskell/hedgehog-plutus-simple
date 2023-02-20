{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.Adjunction (
  Adjunction (Adjunction, lower, raise),
  adjunctionTest,
) where

import Data.Kind (Type)

import Hedgehog ((===))
import Hedgehog qualified

data Adjunction b a = Adjunction
  { lower :: !(a -> b)
  , raise :: !(b -> a)
  }

instance Category Adjunction where
  Adjunction ll lr . Adjunction rl rr =
    Adjunction
      { lower = rl . ll
      , raise = lr . rr
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
  Adjunction b a ->
  a ->
  m ()
adjunctionTest (Adjunction lower raise) a = a === raise (lower a)

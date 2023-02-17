{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hedgehog.Plutus.TestData where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import GHC.Generics qualified as GHC

import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)

import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.Generics

type Bad :: Type -> Type
newtype Bad a = Bad {getBad :: a}

type TestData :: Type -> Constraint
class TestData a where
  type Good a
  type Good a = a

  validate :: a -> Maybe (Good a)

  generalise :: Good a -> a

testDataAdjunction ::
  (TestData a) =>
  Adjunction a (Either (Bad a) (Good a))
testDataAdjunction =
  Adjunction
    { lower = either (.getBad) generalise
    , raise = \g -> maybe (Left (Bad g)) Right (validate g)
    }

instance TestData (Simple a) where
  type Good (Simple a) = a

  validate = Just . coerce
  generalise = coerce

type ShouldEqual :: Type -> Type -> Type
newtype ShouldEqual val a = MightNotEqual a

instance (Eq a, ShouldBeEqualTo val a) => TestData (ShouldEqual val a) where
  type Good (ShouldEqual val a) = ()

  validate (MightNotEqual gen)
    | gen == val (Proxy @val) = Just ()
    | otherwise = Nothing

  generalise () = MightNotEqual $ val (Proxy @val)

type ShouldBeEqualTo :: Type -> Type -> Constraint
class ShouldBeEqualTo val a | val -> a where
  val :: Proxy val -> a

type Mempty :: Type -> Type
data Mempty a

instance (Monoid a) => ShouldBeEqualTo (Mempty a) a where val _ = mempty

instance (TestData good) => TestData (Either bad good) where
  type Good (Either bad good) = Good good

  validate (Left _) = Nothing
  validate (Right good) = validate good

  generalise = Right . generalise

eitherOr :: (b -> a) -> Either a b -> a
eitherOr _ (Left a) = a
eitherOr f (Right b) = f b

shouldBeSingletonList :: [a] -> Either [a] a
shouldBeSingletonList [a] = Right a
shouldBeSingletonList as = Left as

instance TestData (Maybe a) where
  type Good (Maybe a) = ()

  validate Nothing = Just ()
  validate (Just _) = Nothing

  generalise () = Nothing

newtype ShouldBeNatural i = MightBeNegative i

instance (Integral i) => TestData (ShouldBeNatural i) where
  type Good (ShouldBeNatural i) = Natural

  validate (MightBeNegative i)
    | i >= 0 = Just (fromIntegral i)
    | otherwise = Nothing

  generalise = MightBeNegative . fromIntegral

newtype Good' a = Good {unGood' :: Good a}

instance
  ( GHC.Generic a
  , SOP.GFrom a
  , SOP.GTo a
  , SOP.All2 TestData (SOP.GCode a)
  ) =>
  TestData (Generically a)
  where
  type Good (Generically a) = SOP.SOP Good' (SOP.GCode a)

  validate (Generically a) =
    SOP.hctraverse' (Proxy @TestData) (\(SOP.I gen) -> Good <$> validate gen) $
      SOP.gfrom a

  generalise =
    Generically
      . SOP.gto
      . SOP.hcmap (Proxy @TestData) (SOP.I . generalise . (.unGood'))

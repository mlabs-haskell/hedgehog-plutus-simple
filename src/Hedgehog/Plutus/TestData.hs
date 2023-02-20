{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hedgehog.Plutus.TestData (
  Bad,
  Good,
  TestData (validate, generalise),
  test,
  testDataAdjunction,
  EitherOr,
  eitherOr,
  shouldBeSingletonList,
  Shouldn'tExist (MaybeExists, maybeExists),
  shouldBe,
  ShouldEqual (MightNotEqual),
  ShouldBeEqualTo,
  Mempty,
  ShouldBeNatural (MightBeNegative),
) where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import GHC.Generics qualified as GHC

import Control.Monad (guard)
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)

import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction, lower, raise))
import Hedgehog.Plutus.Generics (Generically (Generically), Simple (Simple))

type Bad :: Type -> Type
newtype Bad a = Bad {getBad :: a}

type TestData :: Type -> Constraint
class TestData a where
  type Good a
  type Good a = a

  validate :: a -> Maybe (Good a)

  generalise :: Good a -> a

test :: (TestData a) => a -> Either (Bad a) (Good a)
test a = maybe (Left (Bad a)) Right (validate a)

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

{- | A 'TestData' instance representing either a fixed 'bad' value or a
 sub-'TestData' instance for 'good'.
-}
newtype EitherOr bad good = EitherOr (Either bad good)
  deriving newtype (Functor, Applicative, Monad)

instance (TestData good) => TestData (EitherOr bad good) where
  type Good (EitherOr bad good) = Good good

  validate (EitherOr (Left _)) = Nothing
  validate (EitherOr (Right good)) = validate good

  generalise = EitherOr . Right . generalise

eitherOr :: (b -> a) -> EitherOr a b -> a
eitherOr _ (EitherOr (Left a)) = a
eitherOr f (EitherOr (Right b)) = f b

shouldBeSingletonList :: [a] -> EitherOr [a] a
shouldBeSingletonList [a] = EitherOr $ Right a
shouldBeSingletonList as = EitherOr $ Left as

newtype Shouldn'tExist a = MaybeExists {maybeExists :: Maybe a}
  deriving newtype (Functor, Applicative, Monad)

instance TestData (Shouldn'tExist a) where
  type Good (Shouldn'tExist a) = ()

  validate (MaybeExists Nothing) = Just ()
  validate (MaybeExists (Just _)) = Nothing

  generalise () = MaybeExists Nothing

shouldBe :: (Eq a) => a -> a -> Shouldn'tExist a
shouldBe exp act = MaybeExists (guard (exp == act) >> Just act)

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

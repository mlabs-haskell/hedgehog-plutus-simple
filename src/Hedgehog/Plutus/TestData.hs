{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hedgehog.Plutus.TestData (
  Bad,
  Good,
  Good' (G),
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
  HKD (HKD),
  I (I),
) where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import GHC.Generics qualified as GHC

import Control.Monad (guard)
import Numeric.Natural (Natural)

import Generics.SOP
import Generics.SOP.GGP

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction, lower, raise))
import Hedgehog.Plutus.Generics (Simple (Simple))

type Bad :: Type -> Type
newtype Bad a = Bad {getBad :: a}
  deriving stock (Eq, Show)

type TestData :: Type -> Constraint
class TestData a where
  type Good a

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
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)
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
  deriving stock (Eq, Show)
  deriving newtype (Functor, Applicative, Monad)

instance TestData (Shouldn'tExist a) where
  type Good (Shouldn'tExist a) = ()

  validate (MaybeExists Nothing) = Just ()
  validate (MaybeExists (Just _)) = Nothing

  generalise () = MaybeExists Nothing

shouldBe :: (Eq a) => a -> a -> Shouldn'tExist a
shouldBe exp act = MaybeExists (guard (exp == act) >> Just act)

newtype ShouldBeNatural i = MightBeNegative i
  deriving stock (Eq, Show)

instance (Integral i) => TestData (ShouldBeNatural i) where
  type Good (ShouldBeNatural i) = Natural

  validate (MightBeNegative i)
    | i >= 0 = Just (fromIntegral i)
    | otherwise = Nothing

  generalise = MightBeNegative . fromIntegral

newtype Good' a = G {unGood' :: Good a}

deriving stock instance (Eq (Good a)) => Eq (Good' a)
deriving stock instance (Show (Good a)) => Show (Good' a)

newtype HKD d = HKD (d I)
  deriving stock (GHC.Generic)

deriving stock instance (Eq (d I)) => Eq (HKD d)
deriving stock instance (Show (d I)) => Show (HKD d)

instance
  forall d (xss :: [[Type]]).
  ( GHC.Generic (d I)
  , GHC.Generic (d Good')
  , GFrom (d Good')
  , GFrom (d I)
  , GTo (d Good')
  , GTo (d I)
  , All2 TestData xss
  , AllZip2 G1 (GCode (d Good')) xss
  , AllZip2 G2 xss (GCode (d I))
  , SListI2 (GCode (d Good'))
  , AllZip2 V1 xss (GCode (d Good'))
  , AllZip2 V2 (GCode (d I)) xss
  ) =>
  TestData (HKD d)
  where
  type Good (HKD d) = d Good'

  validate :: HKD d -> Maybe (d Good')
  validate (HKD d) =
    fmap gto
      . hsequence
      . htrans (Proxy @V1) (fmap G . validate . unI)
      . id @(->) @(SOP I xss)
      . htrans (Proxy @V2) (mapII unI)
      . gfrom
      $ d

  generalise :: d Good' -> HKD d
  generalise =
    HKD
      . gto
      . htrans (Proxy @G2) (I . I . generalise . (.unGood'))
      . id @(->) @(SOP Good' xss)
      . htrans (Proxy @G1) unI
      . gfrom

class (x ~ Good' y) => G1 x y
instance (x ~ Good' y) => G1 x y

class (I x ~ y, TestData x) => G2 x y
instance (I x ~ y, TestData x) => G2 x y

class (Good' x ~ y, TestData x) => V1 x y
instance (Good' x ~ y, TestData x) => V1 x y

class (x ~ I y) => V2 x y
instance (x ~ I y) => V2 x y

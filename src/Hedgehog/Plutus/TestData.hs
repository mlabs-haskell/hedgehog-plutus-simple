{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hedgehog.Plutus.TestData where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import GHC.Generics qualified as GHC

import Control.Monad (guard)
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)

import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP

import Generics.SOP.NS (sequence'_SOP)
import Hedgehog.Plutus.Adjunction

type Quality :: Type
data Quality = IsGeneralised | IsGood

type Bad :: (Quality -> Type) -> Type
newtype Bad a = Bad {getBad :: Generalised a}

type Good :: (Quality -> Type) -> Type
type Good a = a 'IsGood

type Generalised :: (Quality -> Type) -> Type
type Generalised a = a 'IsGeneralised

type TestData :: (Quality -> Type) -> Constraint
class TestData a where
  validate :: Generalised a -> Maybe (Good a)

  generalise :: Good a -> Generalised a

testDataAdjunction ::
  (TestData a) =>
  Adjunction (Generalised a) (Either (Bad a) (Good a))
testDataAdjunction =
  Adjunction
    { lower = either (.getBad) generalise
    , raise = \g -> maybe (Left (Bad g)) Right (validate g)
    }

type ShouldEqual :: Type -> Quality -> Type
data ShouldEqual val q where
  IsEqual :: ShouldEqual val 'IsGood
  MightNotBeEqual :: TypeOf val -> ShouldEqual val 'IsGeneralised

type TypeOf :: Type -> Type
type family TypeOf a

type ShouldBeEqualTo :: Type -> Constraint
class ShouldBeEqualTo val where
  val :: Proxy val -> TypeOf val

instance
  (Eq (TypeOf val), ShouldBeEqualTo val) =>
  TestData (ShouldEqual val)
  where
  validate (MightNotBeEqual a) = guard (a == val (Proxy @val)) >> Just IsEqual
  generalise IsEqual = MightNotBeEqual (val $ Proxy @val)

type Mempty :: Type -> Type
data Mempty a

type instance TypeOf (Mempty a) = a

instance (Monoid a) => ShouldBeEqualTo (Mempty a) where val _ = mempty

type Only :: Type -> Quality -> Type
newtype Only a q = Only a
  deriving stock (GHC.Generic)

instance TestData (Only a) where
  validate = Just . coerce
  generalise = coerce

type EitherOr :: (Quality -> Type) -> (Quality -> Type) -> Quality -> Type
data EitherOr bad gen q where
  Shouldn'tBe :: bad 'IsGeneralised -> EitherOr bad gen 'IsGeneralised
  ShouldBe :: gen q -> EitherOr bad gen q

instance (TestData gen) => TestData (EitherOr bad gen) where
  validate (Shouldn'tBe _) = Nothing
  validate (ShouldBe gen) = ShouldBe <$> validate gen

  generalise (ShouldBe good) = ShouldBe (generalise good)

type Predicated :: Type -> Quality -> Type
newtype Predicated pred q = Predicated (TypeOf pred)

class IsPredicated pred where
  predicate :: Proxy pred -> TypeOf pred -> Bool

instance (IsPredicated pred) => TestData (Predicated pred) where
  validate (Predicated a) =
    guard (predicate (Proxy @pred) a) >> Just (Predicated a)

  generalise = coerce

type ShouldBeNatural :: Quality -> Type
data ShouldBeNatural q where
  MightBeNegative :: Integer -> ShouldBeNatural 'IsGeneralised
  IsNatural :: Natural -> ShouldBeNatural 'IsGood

instance TestData ShouldBeNatural where
  validate (MightBeNegative i) =
    guard (i >= 0) >> Just (IsNatural $ fromIntegral i)

  generalise (IsNatural n) = MightBeNegative $ toInteger n

type Shouldn'tBePresent :: (Quality -> Type) -> Quality -> Type
data Shouldn'tBePresent bad q where
  IsAbsent :: Shouldn'tBePresent bad q
  IsPresent :: bad 'IsGeneralised -> Shouldn'tBePresent bad 'IsGeneralised

instance TestData (Shouldn'tBePresent bad) where
  validate IsAbsent = Just IsAbsent
  validate (IsPresent _) = Nothing

  generalise IsAbsent = IsAbsent

expect :: (Eq a) => a -> a -> Generalised (Shouldn'tBePresent (Only a))
expect exp act = if exp == act then IsAbsent else IsPresent (Only act)

type Generically' :: (Quality -> Type) -> Quality -> Type
newtype Generically' f a where
  Generically :: f a -> Generically' f a

class
  ( (forall q. GHC.Generic (a q))
  , SOP.GFrom (Generalised a)
  , SOP.GFrom (Good a)
  , SOP.GTo (Generalised a)
  , SOP.GTo (Good a)
  , SOP.AllZip2
      TestData'
      (SOP.GCode (Generalised a))
      (UnAp2 (SOP.GCode (Good a)))
  , SOP.AllZip2
      (GetAp 'IsGood)
      (UnAp2 (SOP.GCode (Good a)))
      (SOP.GCode (Good a))
  , SOP.AllZip2
      (GetAp 'IsGeneralised)
      (UnAp2 (SOP.GCode (Generalised a)))
      (SOP.GCode (Generalised a))
  , SOP.AllZip2
      (Apped 'IsGood)
      (SOP.GCode (Good a))
      (UnAp2 (SOP.GCode (Good a)))
  , SOP.AllZip2
      Gen
      (UnAp2 (SOP.GCode (Good a)))
      (UnAp2 (SOP.GCode (Generalised a)))
  ) =>
  ValidateGeneric a
instance
  ( (forall q. GHC.Generic (a q))
  , SOP.GFrom (Generalised a)
  , SOP.GFrom (Good a)
  , SOP.GTo (Generalised a)
  , SOP.GTo (Good a)
  , SOP.AllZip2
      TestData'
      (SOP.GCode (Generalised a))
      (UnAp2 (SOP.GCode (Good a)))
  , SOP.AllZip2
      (GetAp 'IsGood)
      (UnAp2 (SOP.GCode (Good a)))
      (SOP.GCode (Good a))
  , SOP.AllZip2
      (GetAp 'IsGeneralised)
      (UnAp2 (SOP.GCode (Generalised a)))
      (SOP.GCode (Generalised a))
  , SOP.AllZip2
      (Apped 'IsGood)
      (SOP.GCode (Good a))
      (UnAp2 (SOP.GCode (Good a)))
  , SOP.AllZip2
      Gen
      (UnAp2 (SOP.GCode (Good a)))
      (UnAp2 (SOP.GCode (Generalised a)))
  ) =>
  ValidateGeneric a

instance (ValidateGeneric a) => TestData (Generically' a) where
  validate (Generically a) =
    fmap
      ( Generically
          . SOP.gto
          . sopGetAp @'IsGood (Proxy @a)
      )
      . sequence'_SOP
      . SOP.htrans
        (Proxy @TestData')
        (SOP.Comp . fmap Ap . validate . SOP.unI)
      $ SOP.gfrom a

  generalise (Generically a) =
    Generically
      . SOP.gto
      . sopGetAp @'IsGeneralised (Proxy @a)
      . SOP.htrans (Proxy @Gen) (Ap . generalise . (.getAp))
      . sopAp @'IsGood (Proxy @a)
      $ SOP.gfrom a

type Ap :: k -> (k -> Type) -> Type
newtype Ap a f = Ap {getAp :: f a}

sopGetAp ::
  forall a f.
  (SOP.AllZip2 (GetAp a) (UnAp2 (SOP.GCode (f a))) (SOP.GCode (f a))) =>
  Proxy f ->
  SOP.SOP (Ap a) (UnAp2 (SOP.GCode (f a))) ->
  SOP.SOP SOP.I (SOP.GCode (f a))
sopGetAp _ = SOP.htrans (Proxy @(GetAp a)) (SOP.I . (.getAp))

sopAp ::
  forall a f.
  (SOP.AllZip2 (Apped a) (SOP.GCode (f a)) (UnAp2 (SOP.GCode (f a)))) =>
  Proxy f ->
  SOP.SOP SOP.I (SOP.GCode (f a)) ->
  SOP.SOP (Ap a) (UnAp2 (SOP.GCode (f a)))
sopAp _ = SOP.htrans (Proxy @(Apped a)) (Ap . SOP.unI)

type GetAp :: k1 -> (k1 -> k2) -> k2 -> Constraint
class (y ~ x a) => GetAp a x y
instance (y ~ x a) => GetAp a x y

type Apped :: k1 -> k2 -> (k1 -> k2) -> Constraint
class (x ~ y a) => Apped a x y
instance (x ~ y a) => Apped a x y

type UnAp2 :: [[a]] -> [[k -> a]]
type family UnAp2 xss where
  UnAp2 (a ': as) = UnAp a ': UnAp2 as
  UnAp2 '[] = '[]

type UnAp :: [a] -> [k -> a]
type family UnAp xs where
  UnAp (f a ': as) = f ': UnAp as
  UnAp '[] = '[]

type TestData' :: Type -> (Quality -> Type) -> Constraint
class (x ~ y 'IsGeneralised, TestData y) => TestData' x y
instance (x ~ y 'IsGeneralised, TestData y) => TestData' x y

type Gen :: (Quality -> Type) -> (Quality -> Type) -> Constraint
class (x ~ y, TestData y) => Gen x y
instance (x ~ y, TestData y) => Gen x y

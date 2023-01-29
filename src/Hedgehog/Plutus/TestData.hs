{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.TestData where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import GHC.Generics qualified as GHC

import Control.Monad (guard)
import Data.Proxy (Proxy (Proxy))

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

type TestData :: Type -> (Quality -> Type) -> Constraint
class TestData ctx a | a -> ctx where
  validate :: ctx -> Generalised a -> Maybe (Good a)

  generalise :: Good a -> Generalised a

  default validate ::
    (Coercible (Generalised a) (Good a)) =>
    ctx ->
    Generalised a ->
    Maybe (Good a)
  validate _ = Just . coerce

  default generalise ::
    (Coercible (Good a) (Generalised a)) =>
    Good a ->
    Generalised a
  generalise = coerce

testDataAdjunction ::
  (TestData ctx a) =>
  ctx ->
  Adjunction (Generalised a) (Either (Bad a) (Good a))
testDataAdjunction ctx =
  Adjunction
    { lower = either getBad generalise
    , raise = \g -> maybe (Left (Bad g)) Right (validate ctx g)
    }

type ShouldEqual :: Type -> Type -> Quality -> Type
data ShouldEqual val ctx q where
  IsEqual :: ShouldEqual val ctx 'IsGood
  MightNotBeEqual :: TypeOf val -> ShouldEqual val ctx 'IsGeneralised

type TypeOf :: Type -> Type
type family TypeOf a

type ShouldBeEqualTo :: Type -> Constraint
class ShouldBeEqualTo val where
  val :: Proxy val -> TypeOf val

instance
  (Eq (TypeOf val), ShouldBeEqualTo val) =>
  TestData ctx (ShouldEqual val ctx)
  where
  validate _ (MightNotBeEqual a) = guard (a == val (Proxy @val)) >> Just IsEqual
  generalise IsEqual = MightNotBeEqual (val $ Proxy @val)

type Mempty :: Type -> Type
data Mempty a

type instance TypeOf (Mempty a) = a

instance (Monoid a) => ShouldBeEqualTo (Mempty a) where val _ = mempty

type Only :: Type -> Type -> Quality -> Type
newtype Only a ctx q = Only a
  deriving anyclass (TestData ctx)

type Predicated :: Type -> Type -> Quality -> Type
newtype Predicated pred ctx q = Predicated (TypeOf pred)

class IsPredicated pred where
  predicate :: Proxy pred -> TypeOf pred -> Bool

instance (IsPredicated pred) => TestData ctx (Predicated pred ctx) where
  validate _ (Predicated a) =
    guard (predicate (Proxy @pred) a) >> Just (Predicated a)

type Generically' :: Type -> (Quality -> Type) -> (Quality -> Type)
newtype Generically' ctx f a where
  Generically :: f a -> Generically' ctx f a

instance
  ( (forall q. GHC.Generic (a q))
  , SOP.GFrom (Generalised a)
  , SOP.GFrom (Good a)
  , SOP.GTo (Generalised a)
  , SOP.GTo (Good a)
  , SOP.AllZip2
      (TestData' ctx)
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
      (Gen ctx)
      (UnAp2 (SOP.GCode (Good a)))
      (UnAp2 (SOP.GCode (Generalised a)))
  ) =>
  TestData ctx (Generically' ctx a)
  where
  validate ctx (Generically a) =
    fmap
      ( Generically
          . SOP.gto
          . sopGetAp @'IsGood (Proxy @a)
      )
      . sequence'_SOP
      . SOP.htrans
        (Proxy @(TestData' ctx))
        (SOP.Comp . fmap Ap . validate ctx . SOP.unI)
      $ SOP.gfrom a

  generalise (Generically a) =
    Generically
      . SOP.gto
      . sopGetAp @'IsGeneralised (Proxy @a)
      . SOP.htrans (Proxy @(Gen ctx)) (Ap . generalise . getAp)
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
sopGetAp _ = SOP.htrans (Proxy @(GetAp a)) (SOP.I . getAp)

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

type TestData' :: Type -> Type -> (Quality -> Type) -> Constraint
class
  ( x ~ y 'IsGeneralised
  , TestData ctx y
  ) =>
  TestData' ctx x y
instance
  ( x ~ y 'IsGeneralised
  , TestData ctx y
  ) =>
  TestData' ctx x y

type Gen :: Type -> (Quality -> Type) -> (Quality -> Type) -> Constraint
class (x ~ y, TestData ctx y) => Gen ctx x y
instance (x ~ y, TestData ctx y) => Gen ctx x y

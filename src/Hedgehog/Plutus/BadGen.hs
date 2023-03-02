{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.BadGen where

import Data.Bifunctor (Bifunctor (bimap))

import GHC.Float (int2Double)
import GHC.Generics qualified as GHC

import Generics.SOP (
  All,
  All2,
  HApInjs (hapInjs),
  HCollapse (hcollapse),
  HPure (hcpure),
  HSequence (hsequence'),
  I,
  K (K),
  NP,
  Proxy (Proxy),
  SOP (SOP),
  hcmap,
  hctraverse,
  type (:.:) (Comp),
 )
import Generics.SOP.GGP (GCode, GTo, gto)

import Hedgehog qualified
import Hedgehog.Gen qualified as Hedgehog

import Generics.SOP.Dict (Dict (Dict), mapAll)
import Hedgehog.Plutus.Generics (Generically, Simple (Simple))
import Hedgehog.Plutus.TestData (
  Bad,
  EitherOr (EitherOr),
  ShouldBeNatural (MightBeNegative),
  ShouldEqual (MightNotEqual),
  Shouldn'tExist (MaybeExists),
  TestData (Good, generalise),
  test,
 )

class TestGen a where
  mutationPoints :: Proxy a -> Double

badGen :: (Hedgehog.MonadGen m, TestData a) => m a -> m (Bad a)
badGen = Hedgehog.mapMaybeT (either Just (const Nothing) . test)

instance TestGen (Simple a) where
  mutationPoints _ = 0

simpleGen :: (Functor m) => m a -> m (Simple a)
simpleGen = fmap Simple

instance TestGen (ShouldEqual val a) where
  mutationPoints _ = 1

shouldEqualGen :: (Functor m) => m a -> m (ShouldEqual val a)
shouldEqualGen = fmap MightNotEqual

instance (TestGen good) => TestGen (EitherOr bad good) where
  mutationPoints _ = 1 + mutationPoints (Proxy @good)

eitherOrGen ::
  forall m bad good.
  (TestGen good, Hedgehog.MonadGen m) =>
  m bad ->
  m good ->
  m (EitherOr bad good)
eitherOrGen bad good =
  EitherOr
    <$> Hedgehog.frequency
      [ (ceiling (1 / (points + 1)), Left <$> bad)
      , (ceiling (points / (points + 1)), Right <$> good)
      ]
  where
    points = mutationPoints (Proxy @good)

instance TestGen (Shouldn'tExist a) where
  mutationPoints _ = 1

shouldntExistGen :: (Functor m) => m a -> m (Shouldn'tExist a)
shouldntExistGen = fmap (MaybeExists . Just)

instance TestGen (ShouldBeNatural a) where
  mutationPoints _ = 1

shouldBeNaturalGen :: (Functor m) => m i -> m (ShouldBeNatural i)
shouldBeNaturalGen = fmap MightBeNegative

instance (All2 TestGen (GCode a)) => TestGen (Generically a) where
  mutationPoints _ = nsPoints (Proxy @(GCode a))
    where
      nsPoints :: forall k (xss :: [[k]]). (All2 TestGen xss) => Proxy xss -> Double
      nsPoints _ =
        mean
          . hcollapse @[k] @[[k]] @NP @xss @Double
          $ hcpure (Proxy @(All TestGen)) npPoints'
        where
          mean as =
            let (cnt, sum) = foldr (\a -> bimap (+ 1) (+ a)) (0, 0) as
             in sum / cnt

          npPoints' :: forall xs. (All TestGen xs) => K Double xs
          npPoints' = K $ npPoints (Proxy @xs)

npPoints :: forall k (xs :: [k]). (All TestGen xs) => Proxy xs -> Double
npPoints _ =
  sum
    . hcollapse @k @[k] @NP @xs @Double
    $ hcpure (Proxy @TestGen) genPoints
  where
    genPoints :: forall x. (TestGen x) => K Double x
    genPoints = K $ mutationPoints (Proxy @x)

data Gens m a = Gens
  { goodGen :: m (Good a)
  , genGen :: m a
  }

class (TestData a, TestGen a) => Testing a

genericGen ::
  ( All2 Testing (GCode a)
  , GHC.Generic a
  , GTo a
  , Hedgehog.MonadGen m
  ) =>
  NP (NP (Gens m)) (GCode a) ->
  m a
genericGen =
  Hedgehog.choice
    . fmap (fmap (gto . SOP) . hsequence')
    . hapInjs
    . hcmap (Proxy @(All Testing)) (Comp . npGen)

npGen ::
  forall m xs.
  (All Testing xs, Hedgehog.MonadGen m) =>
  NP (Gens m) xs ->
  m (NP I xs)
npGen = hctraverse (Proxy @Testing) go
  where
    go ::
      forall x.
      (Testing x, Hedgehog.MonadGen m) =>
      Gens m x ->
      m x
    go Gens {goodGen, genGen} =
      Hedgehog.sized $ \(Hedgehog.Size i) ->
        let fieldPoints :: Double
            fieldPoints = mutationPoints (Proxy @x) * int2Double i
         in Hedgehog.frequency
              [ (ceiling (fieldPoints / totalPoints), genGen)
              ,
                ( floor ((totalPoints - fieldPoints) / totalPoints)
                , generalise <$> goodGen
                )
              ]

    totalPoints :: Double
    totalPoints = case mapAll @Testing @TestGen @xs (\Dict -> Dict) Dict of
      Dict -> npPoints (Proxy @xs)

{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.Diff where

import GHC.Generics qualified as GHC

import Data.Coerce (coerce)

import Generics.SOP
import Generics.SOP.GGP

import Hedgehog.Plutus.Generics

class Diff' a where
  type Patch a

  diff' :: a -> a -> Patch a

  patch' :: Patch a -> a -> a

class (Eq a, Diff' a) => Diff a
instance (Eq a, Diff' a) => Diff a

diff :: (Diff a) => a -> a -> Maybe (Patch a)
diff a b
  | a == b = Nothing
  | otherwise = Just (diff' a b)

patch :: (Diff' a) => Maybe (Patch a) -> a -> a
patch mp c = maybe c (`patch'` c) mp

instance Diff' (Simple a) where
  type Patch (Simple a) = a

  diff' _ = coerce
  patch' b _ = Simple b

newtype Patch' a = Patch' (Maybe (Patch a))

instance (All Diff as) => Diff' (NP I as) where
  type Patch (NP I as) = NP Patch' as

  diff' = hcliftA2 (Proxy @Diff) (\(I a) (I b) -> Patch' $ diff a b)
  patch' = hcliftA2 (Proxy @Diff) (\(Patch' p) (I c) -> I $ patch p c)

data ConsPatch xs = ConsPatch (NP I xs) (Maybe (Patch (NP I xs)))

instance (All2 Diff xss) => Diff' (NS (NP I) xss) where
  type Patch (NS (NP I) xss) = NS ConsPatch xss

  diff' ass bss =
    maybe
      (hmap (`ConsPatch` Nothing) bss)
      ( hcmap
          (Proxy @(All Diff))
          (\(Pair as bs) -> ConsPatch bs (Just $ diff' as bs))
      )
      $ sameCons ass bss
    where
      sameCons :: NS f as -> NS f as -> Maybe (NS (Pair f) as)
      sameCons (Z a) (Z b) = Just $ Z (Pair a b)
      sameCons (S as) (S bs) = S <$> sameCons as bs
      sameCons _ _ = Nothing

  patch' ((Z (ConsPatch _ (Just p)))) (Z c) = Z $ patch' p c
  patch' ((S ps)) (S cs) = S $ patch' ps cs
  patch' ps _ = hmap (\(ConsPatch bs _) -> bs) ps

data Pair f a = Pair (f a) (f a)

newtype SOPPatch a = SOPPatch (NS ConsPatch (GCode a))

instance
  (GHC.Generic a, GFrom a, GTo a, All2 Diff (GCode a)) =>
  Diff' (Generically a)
  where
  type Patch (Generically a) = SOPPatch a

  diff' (Generically a) (Generically b) =
    SOPPatch $
      diff'
        (unSOP . gfrom $ a)
        (unSOP . gfrom $ b)

  patch' (SOPPatch p) (Generically c) =
    Generically . gto . SOP $
      patch' p (unSOP . gfrom $ c)

deriving via (Simple Integer) instance Diff' Integer

deriving via (Generically (Maybe a)) instance (Eq a, Diff' a) => Diff' (Maybe a)

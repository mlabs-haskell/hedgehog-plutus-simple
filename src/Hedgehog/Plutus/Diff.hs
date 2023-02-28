{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Plutus.Diff (
  Diff' (diff', patch'),
  Diff,
  Patch,
  diff,
  patch,
) where

import GHC.Generics qualified as GHC

import Data.Coerce (coerce)

import Generics.SOP (
  All,
  All2,
  Compose,
  I (I),
  NP,
  NS (S, Z),
  Proxy (Proxy),
  SOP (SOP),
  hcliftA2,
  hcmap,
  hmap,
  unSOP,
 )
import Generics.SOP.GGP (GCode, GFrom, GTo, gfrom, gto)

import Hedgehog.Plutus.Generics (Generically (Generically), Simple (Simple))
import PlutusLedgerApi.V1 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName,
  Value,
 )
import PlutusTx.Monoid (Group (inv))

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

deriving stock instance (Eq (Patch a)) => Eq (Patch' a)
deriving stock instance (Show (Patch a)) => Show (Patch' a)

instance (All Diff as) => Diff' (NP I as) where
  type Patch (NP I as) = NP Patch' as

  diff' = hcliftA2 (Proxy @Diff) (\(I a) (I b) -> Patch' $ diff a b)
  patch' = hcliftA2 (Proxy @Diff) (\(Patch' p) (I c) -> I $ patch p c)

data ConsPatch xs = ConsPatch (NP I xs) (Maybe (Patch (NP I xs)))

deriving stock instance
  ( All (Compose Eq I) xs
  , All (Compose Eq Patch') xs
  ) =>
  Eq (ConsPatch xs)
deriving stock instance
  ( All (Compose Show I) xs
  , All (Compose Show Patch') xs
  ) =>
  Show (ConsPatch xs)

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

deriving stock instance
  (All (Compose Eq ConsPatch) (GCode a)) =>
  Eq (SOPPatch a)

deriving stock instance
  (All (Compose Show ConsPatch) (GCode a)) =>
  Show (SOPPatch a)

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
deriving via (Simple PubKeyHash) instance Diff' PubKeyHash
deriving via (Simple POSIXTime) instance Diff' POSIXTime
deriving via (Simple CurrencySymbol) instance Diff' CurrencySymbol
deriving via (Simple TokenName) instance Diff' TokenName

-- deriving via (Simple Value) instance Diff' Value
-- TODO an instance that actually subtracts would be better

instance Diff' Value where
  type Patch Value = Value
  diff' a b = a <> inv b
  patch' a b = a <> b

deriving via (Generically (Maybe a)) instance (Eq a, Diff' a) => Diff' (Maybe a)

module LiquidityBridge (exampleFunction) where

import Plutarch.Monadic qualified as P

exampleFunction :: Term s (PInteger :--> PInteger)
exampleFunction = plam $ \x -> P.do
  x' <- plet $ x + 1 + 2 + 3
  x' + x'

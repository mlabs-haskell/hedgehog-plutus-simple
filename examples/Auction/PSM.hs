module Auction.PSM
  ( bid
  , start
  , end
  , addUser
  , getBals
  ) where

import Plutus.Model.V2 (Run, adaValue, newUser, getLovelace, adaOf)

import PlutusLedgerApi.V2 (PubKeyHash)
import Plutus.Model (valueAt)
import Control.Monad (forM)

addUser :: Int -> Run PubKeyHash
addUser = newUser . adaValue . fromIntegral

getBals :: [PubKeyHash] -> Run [(PubKeyHash,Int)]
getBals keys = (zip keys <$>) $ forM keys $ \key -> do
    val <- valueAt key
    pure $ fromIntegral $ getLovelace $ adaOf val

start :: Run ()
start = pure ()

bid :: PubKeyHash -> Int -> Run ()
bid _ _ = pure ()

end :: Run ()
end = pure ()

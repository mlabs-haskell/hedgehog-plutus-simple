module Auction.PSM
  ( bid
  , end
  , addUser
  ) where

import Plutus.Model (Run, newUser)

import Plutus.Model.V2 (adaValue)
import PlutusLedgerApi.V1 (PubKeyHash)

addUser :: Int -> Run PubKeyHash
addUser = newUser . adaValue . fromIntegral

bid :: PubKeyHash -> Int -> Run ()
bid _ _ = pure ()

end :: Run ()
end = pure ()

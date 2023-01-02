module Auction.PSM (
  bid,
  start,
  end,
  addUser,
  getBals,
) where

import Control.Monad (forM)
import PlutusLedgerApi.V2 (PubKeyHash)

import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  Run,
  TypedValidator (TypedValidator),
  adaOf,
  adaValue,
  getLovelace,
  newUser,
  payToScript,
  submitTx,
  toV2,
  valueAt,
 )

addUser :: Int -> Run PubKeyHash
addUser = newUser . adaValue . fromIntegral

getBals :: [PubKeyHash] -> Run [(PubKeyHash, Int)]
getBals keys = (zip keys <$>) $
  forM keys $ \key -> do
    val <- valueAt key
    pure $ fromIntegral $ getLovelace $ adaOf val

start :: PubKeyHash -> Run ()
start pkh = do
  let tx =
        payToScript @Auction
          (TypedValidator $ toV2 $ undefined)
          -- It seems like it's not possible to use plutarch here
          (InlineDatum Nothing)
          (adaValue 1_000_000)
  outs <- getOutputs tx
  submitTx pkh tx
  pure outs
  where
    getOutputs = undefined

-- The internal implementation
-- in psm seems to require hidden functions

bid :: PubKeyHash -> Int -> Run ()
bid _ _ = pure ()

end :: Run ()
end = pure ()

type Auction =
  TypedValidator (Maybe ()) ()

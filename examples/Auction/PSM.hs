module Auction.PSM (
  bid,
  start,
  end,
  addUser,
  getBals,
) where

import Control.Monad (forM)
import Data.Default (def)
import Data.Text (unpack)

import Plutarch.Prelude (PUnit (PUnit), pcon, plam)

import Plutus.Model.Validator.V2 (mkTypedValidatorPlutarch)
import PlutusLedgerApi.V2 (PubKeyHash)

import Plutus.Model (utxoAt)
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  Run,
  TypedValidator,
  adaOf,
  adaValue,
  getLovelace,
  newUser,
  payToScript,
  spend,
  submitTx,
  userSpend,
  valueAt,
 )
import PlutusLedgerApi.V1 (TxOutRef)

addUser :: Int -> Run PubKeyHash
addUser = newUser . adaValue . fromIntegral

getBals :: [PubKeyHash] -> Run [(PubKeyHash, Int)]
getBals keys = (zip keys <$>) $
  forM keys $ \key -> do
    val <- valueAt key
    pure $ fromIntegral $ getLovelace $ adaOf val

start :: PubKeyHash -> Run TxOutRef
start pkh = do
  auctionValidator <- getAuctionValidator
  let val = adaValue 1_000
  us <- spend pkh val
  let
    tx =
      userSpend us
        <> payToScript @Auction
          auctionValidator
          (InlineDatum Nothing)
          val
  submitTx pkh tx
  utxos <- utxoAt auctionValidator
  pure $ fst $ head $ utxos

-- TODO it would be nice if PSM could give you more info
-- on submitTx ie. outputs with txids

-- The internal implementation
-- in psm seems to require hidden functions

bid :: PubKeyHash -> Int -> Run ()
bid _ _ = pure ()

end :: Run ()
end = pure ()

type Auction =
  TypedValidator (Maybe ()) ()

getAuctionValidator :: Run Auction
getAuctionValidator =
  case mkTypedValidatorPlutarch def (plam $ \_ _ _ -> pcon PUnit) of
    Right v -> pure v
    Left t -> fail $ unpack t

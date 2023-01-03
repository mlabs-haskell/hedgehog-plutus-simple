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
import PlutusLedgerApi.V2 (PubKeyHash, singleton)

import Plutus.Model (mintValue, utxoAt)
import Plutus.Model.V1 (
  DatumMode (HashDatum),
  Run,
  TypedValidator,
  adaOf,
  adaValue,
  getLovelace,
  newUser,
  payToScript,
  -- spend,
  submitTx,
  -- userSpend,
  valueAt,
 )
import PlutusLedgerApi.V1 (TxOutRef)

-- import PlutusLedgerApi.V2.Value (leq)

import Plutus.Model.V1 (TypedPolicy, scriptCurrencySymbol)
import Plutus.Model.Validator.V1 (mkTypedPolicyPlutarch)

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
  mp <- getTrivialMP
  let cs = scriptCurrencySymbol mp
  let val = singleton cs "" 1
  -- us <- spend pkh val
  let
    tx =
      -- userSpend us
      mintValue mp () val
        <> payToScript @Auction
          auctionValidator
          (HashDatum Nothing)
          -- TODO I'd like to use inline datum
          -- but V2 causes an error for some reason
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

type TrivialToken = TypedPolicy ()

getTrivialMP :: Run TrivialToken
getTrivialMP =
  case mkTypedPolicyPlutarch def (plam $ \_ _ -> pcon PUnit) of
    Right v -> pure v
    Left t -> fail $ unpack t

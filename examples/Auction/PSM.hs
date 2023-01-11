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

import Plutus.Model (mintValue, payToKey, utxoAt)
import Plutus.Model.V1 (
  DatumMode (HashDatum),
  Run,
  TypedPolicy,
  TypedValidator,
  adaOf,
  adaValue,
  getLovelace,
  newUser,
  payToScript,
  scriptCurrencySymbol,
  spend,
  spendScript,
  submitTx,
  userSpend,
  valueAt,
 )
import Plutus.Model.Validator.V1 (mkTypedPolicyPlutarch, mkTypedValidatorPlutarch)
import PlutusLedgerApi.V1 (PubKeyHash, TxOutRef, singleton)

-- | create a new user with a given balance (in lovelace)
addUser :: Int -> Run PubKeyHash
addUser = newUser . adaValue . fromIntegral

{- | given a list of PubKeyHashs
 returns the lovelace balance at each PubKeyHash
 paired with the PubKeyHash
-}
getBals :: [PubKeyHash] -> Run [(PubKeyHash, Int)]
getBals keys = (zip keys <$>) $
  forM keys $ \key -> do
    val <- valueAt key
    pure $ fromIntegral $ getLovelace $ adaOf val

{- | given a pubKeyHash starts an auction as that user
 returns the txOutRef of the auction utxo
-}
start :: PubKeyHash -> Run TxOutRef
start pkh = do
  auctionValidator <- getAuctionValidator
  mp <- getTrivialMP
  let cs = scriptCurrencySymbol mp
  let val = singleton cs "" 1
  let
    tx =
      mintValue mp () val
        <> payToScript @Auction
          auctionValidator
          (HashDatum Nothing)
          -- TODO I'd like to use inline datum
          -- but V2 causes an error for some reason
          val
  submitTx pkh tx
  utxos <- utxoAt auctionValidator
  pure $ fst $ head utxos

-- TODO it would be nice if PSM could give you more info
-- on submitTx ie. outputs with txids

-- The internal implementation
-- in psm seems to require hidden functions

{- | bid takes the txOutRef of the current auction utxo
 the pubKeyHash of the current bidder
 the new bid amount (in lovelace)
 the previous pubKeyHash to be refunded
 and the amount (in lovelace to refund to that pubKeyHash)
 submits a tx creating a new bid and refunding the previous bidder
 and returning the txoutref of the new auction utxo
-}
bid :: TxOutRef -> PubKeyHash -> Int -> PubKeyHash -> Int -> Run TxOutRef
bid oldRef pkh bidAmt refundPkh refundAmt = do
  auctionValidator <- getAuctionValidator
  let val = singleton "" "" $ fromIntegral bidAmt
  us <- spend pkh val
  mp <- getTrivialMP
  let cs = scriptCurrencySymbol mp
      token = singleton cs "" 1
      tx =
        userSpend us
          <> spendScript
            auctionValidator
            oldRef
            ()
            Nothing
          <> payToKey
            refundPkh
            (singleton "" "" $ fromIntegral refundAmt)
          <> payToScript @Auction
            auctionValidator
            (HashDatum Nothing)
            -- TODO I'd like to use inline datum
            -- but V2 causes an error for some reason
            (val <> token)
  submitTx pkh tx
  utxos <- utxoAt auctionValidator
  pure $ fst $ head utxos

{- | given the auction owner's pubKeyHash
 the auction winer's pubKeyHash
 the final bid amount in lovelace
 and the TxOutRef of the final utxo
 ends the auction paying the final bid to the owner
 and giving the auctioned token to the winner
-}
end :: PubKeyHash -> PubKeyHash -> Int -> TxOutRef -> Run ()
end owner winer amt ref = do
  auctionValidator <- getAuctionValidator
  mp <- getTrivialMP
  let cs = scriptCurrencySymbol mp
      token = singleton cs "" 1
      tx =
        spendScript auctionValidator ref () Nothing
          <> payToKey owner (singleton "" "" $ fromIntegral amt)
          <> payToKey winer token
  submitTx owner tx

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

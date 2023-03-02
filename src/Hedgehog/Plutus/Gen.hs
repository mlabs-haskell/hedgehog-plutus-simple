{-# LANGUAGE DisambiguateRecordFields #-}

module Hedgehog.Plutus.Gen where

import Control.Monad ((>=>))
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify)
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.SafeHash qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus

import Cardano.Simple.Cardano.Common qualified as Simple
import Cardano.Simple.Ledger.Slot qualified as Simple

import Plutus.Model qualified as Model
import Plutus.Model.Stake qualified as Model

data User = User
  { user :: !Model.User
  , userName :: !String
  , userOutputs :: ![Output]
  }

data Output = Output
  { outputRef :: !Plutus.TxOutRef
  , output :: !Plutus.TxOut
  , outputDatum :: !(Maybe Plutus.Datum)
  }

initMockState ::
  forall m.
  (Monad m) =>
  Map String (Plutus.PubKeyHash -> m [(Plutus.TxOut, Maybe Plutus.Datum)]) ->
  Map Plutus.ScriptHash (String, m [(Plutus.TxOut, Maybe Plutus.Datum)]) ->
  Model.MockConfig ->
  m Model.Mock
initMockState users scripts cfg = (`evalStateT` 0) $ do
  us <- users'
  ss <- scripts'
  let outputs =
        concatMap (.userOutputs) (Map.elems us)
          ++ concatMap snd (Map.elems ss)
  pure $
    Model.Mock
      { mockUsers = fmap (.user) us
      , mockUtxos =
          Map.fromList
            . fmap (\Output {outputRef, output} -> (outputRef, output))
            $ outputs
      , mockDatums =
          Map.fromList
            . fmap (\d -> (Model.datumHash d, d))
            $ mapMaybe (.outputDatum) outputs
      , mockAddresses =
          Map.mapKeysMonotonic Plutus.pubKeyHashAddress
            . fmap (Set.fromList . fmap (.outputRef) . (.userOutputs))
            $ us
      , mockStake = initStake
      , mockTxs = mempty
      , mockConfig = cfg
      , mockCurrentSlot = Simple.Slot 1
      , mockUserStep = fromIntegral $ Map.size users
      , mockFails = mempty
      , mockInfo = mempty
      , mustFailLog = mempty
      , mockNames =
          Model.MockNames
            { mockNameUsers = fmap (.userName) us
            , mockNameAddresses =
                Map.mapKeysMonotonic Plutus.scriptHashAddress $
                  fmap fst ss
            , mockNameAssetClasses = Map.empty
            , mockNameCurrencySymbols = Map.empty
            , mockNameTxns = Map.empty
            }
      }
  where
    mkOutputs ::
      m [(Plutus.TxOut, Maybe Plutus.Datum)] ->
      StateT Integer m [Output]
    mkOutputs =
      lift
        >=> traverse
          ( \(o, d) -> do
              i <- get
              modify (+ 1)
              pure
                Output
                  { outputRef = Plutus.TxOutRef genesisTxId i
                  , output = o
                  , outputDatum = d
                  }
          )

    users' :: StateT Integer m (Map Plutus.PubKeyHash User)
    users' =
      fmap Map.fromList
        . traverse
          ( \(i, (name, os)) ->
              do
                let user = Model.intToUser i
                    pkh = Model.userPubKeyHash user
                outputs <- mkOutputs (os pkh)
                pure
                  ( pkh
                  , User
                      { user = user
                      , userName = name
                      , userOutputs = outputs
                      }
                  )
          )
        . zip [0 ..]
        . Map.toList
        $ users

    scripts' :: StateT Integer m (Map Plutus.ScriptHash (String, [Output]))
    scripts' = traverse (traverse mkOutputs) scripts

    genesisTxId :: Plutus.TxId
    genesisTxId =
      Simple.fromTxId
        . Ledger.TxId
        . Ledger.unsafeMakeSafeHash
        . Crypto.castHash
        $ Crypto.hashWith CBOR.serialize' ()

    initStake =
      Model.Stake
        { stake'pools = Map.empty
        , stake'poolIds = Vector.empty
        , stake'stakes = Map.empty
        , stake'nextReward = 0
        }

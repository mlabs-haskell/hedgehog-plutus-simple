module Hedgehog.Plutus.Objective where

import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Cardano.Ledger.Alonzo qualified as Ledger hiding (PParams, Value)
import Cardano.Ledger.Alonzo.TxInfo qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger hiding (PParams)
import Cardano.Ledger.Shelley.TxBody qualified as Ledger
import PlutusLedgerApi.V1 qualified as Plutus hiding (TxOut)
import PlutusLedgerApi.V2.Tx qualified as Plutus
import PlutusTx.AssocMap qualified as Plutus

import Plutus.Model qualified as Model hiding (Tx)
import Plutus.Model.Fork.Cardano.Class qualified as Fork
import Plutus.Model.Fork.Ledger.Tx qualified as Fork
import Plutus.Model.Mock.ProtocolParameters qualified as Model

import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data TxBuilder = TxBuilder
  { chainState :: Model.Mock
  , incrementalTx :: Fork.Tx
  }

availableUtxos :: TxBuilder -> Map Plutus.TxOutRef Plutus.TxOut
availableUtxos
  TxBuilder
    { chainState = Model.Mock {Model.mockUtxos}
    , incrementalTx = Fork.Tx {Fork.txInputs}
    } = Map.withoutKeys mockUtxos (Set.map Fork.txInRef txInputs)

data Objective = Objective
  { objectiveRange :: (Plutus.Value, Plutus.Value)
  , runObjective :: Plutus.Value -> TxBuilder
  }

genObjective :: Objective -> Hedgehog.Gen TxBuilder
genObjective
  Objective
    { objectiveRange = (Plutus.Value l, Plutus.Value r)
    , runObjective
    } =
    runObjective
      . Plutus.Value
      <$> pIntersectionWith
        (pIntersectionWith (\a b -> Gen.integral $ Range.constant a b))
        l
        r
    where
      toMap :: (Ord k) => Plutus.Map k v -> Map k v
      toMap = Map.fromList . Plutus.toList

      fromMap :: (Ord k) => Map k v -> Plutus.Map k v
      fromMap = Plutus.fromList . Map.toList

      pIntersectionWith ::
        (Ord k, Applicative f) =>
        (a -> b -> f v) ->
        Plutus.Map k a ->
        Plutus.Map k b ->
        f (Plutus.Map k v)
      pIntersectionWith f l' r' =
        fmap fromMap . sequenceA $ Map.intersectionWith f (toMap l') (toMap r')

balance :: TxBuilder -> Plutus.Value
balance
  bldr@TxBuilder
    { chainState =
      Model.Mock
        { Model.mockConfig =
          Model.MockConfig
            { Model.mockConfigProtocol
            , Model.mockConfigNetworkId
            }
        }
    , incrementalTx =
      tx@Fork.Tx
        { Fork.txScripts
        , Fork.txInputs
        , Fork.txCollateral
        , Fork.txReferenceInputs
        }
    } =
    case mockConfigProtocol of
      Model.AlonzoParams params -> balance' params
      Model.BabbageParams params -> balance' params
    where
      balance' ::
        ( Ledger.CLI era
        , Ledger.ShelleyEraTxBody era
        , Ledger.Value era ~ Ledger.MaryValue Ledger.StandardCrypto
        , Fork.IsCardanoTx era
        ) =>
        Ledger.PParams era ->
        Plutus.Value
      balance' params =
        Ledger.transValue $
          Ledger.evaluateTransactionBalance
            params
            ( either error id
                . Fork.toUtxo txScripts mockConfigNetworkId
                . fmap
                  ( \inp ->
                      ( inp
                      , fromJust
                          . (`Map.lookup` availableUtxos bldr)
                          $ Fork.txInRef inp
                      )
                  )
                $ ins
            )
            (const True)
            ( either error Fork.getTxBody
                . Fork.toCardanoTx txScripts mockConfigNetworkId params
                . Model.toExtra
                $ tx
            )
        where
          ins = Set.toList $ txInputs <> txCollateral <> txReferenceInputs

walletBalance :: Plutus.PubKeyHash -> TxBuilder -> Plutus.Value
walletBalance = _

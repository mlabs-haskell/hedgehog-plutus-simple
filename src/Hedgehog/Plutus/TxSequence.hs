{-# LANGUAGE MultiWayIf #-}

module Hedgehog.Plutus.TxSequence where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)

import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust)

import Cardano.Ledger.Alonzo qualified as Ledger hiding (PParams, Value)
import Cardano.Ledger.Alonzo.TxInfo qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger hiding (PParams)
import Cardano.Ledger.Shelley.TxBody qualified as Ledger
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Cardano.Class qualified as Fork
import Plutus.Model.Fork.Ledger.Tx qualified as Fork
import Plutus.Model.Mock.ProtocolParameters qualified as Model

import Hedgehog qualified

genTxSequence :: Model.Mock -> Hedgehog.Gen (Vector Model.Tx)
genTxSequence = _

genTx :: Model.Mock -> Hedgehog.Gen Fork.Tx
genTx = _

stepMock :: Fork.Tx -> Model.Mock -> Model.Mock
stepMock tx = snd . Model.runMock (Model.sendTx . Model.toExtra $ tx)

data TxBuilder = TxBuilder
  { chainState :: Model.Mock
  , incrementalTx :: Fork.Tx
  }

newtype TxSample = TxSample
  { unTxSample ::
      Map
        Plutus.CurrencySymbol
        (Map Plutus.TokenName (Plutus.Interval Integer))
  }

data Status
  = -- | PubKey output with no special status, available to be arbitrarily spent
    Spendable
  | -- | unspent output, either script-locked or holding a datum or reference
    -- | script
    Special
  | -- | Already spent by 'incrementalTx'
    Spent
  deriving stock (Eq)

utxoStatus :: TxBuilder -> Plutus.TxOutRef -> Maybe Status
utxoStatus
  TxBuilder
    { chainState = Model.Mock {Model.mockUtxos}
    , incrementalTx = Fork.Tx {Fork.txInputs}
    }
  ref =
    Map.lookup ref mockUtxos <&> \out ->
      if
          | Set.member ref (Set.map Fork.txInRef txInputs) -> Spent
          | or
              ( [ not . Plutus.isPubKeyOut
                , (/= Plutus.NoOutputDatum) . Plutus.txOutDatum
                , isJust . Plutus.txOutReferenceScript
                ]
                  <&> ($ out)
              ) ->
              Special
          | otherwise -> Spendable

builderUtxos :: TxBuilder -> Map Plutus.TxOutRef (Plutus.TxOut, Status)
builderUtxos
  bldr@TxBuilder {chainState = Model.Mock {Model.mockUtxos}} =
    Map.mapWithKey (\ref o -> (o, fromJust $ utxoStatus bldr ref)) mockUtxos

utxosByStatus ::
  (Status -> Bool) ->
  TxBuilder ->
  Map Plutus.TxOutRef Plutus.TxOut
utxosByStatus p = Map.map fst . Map.filter (p . snd) . builderUtxos

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
                          . (`Map.lookup` utxosByStatus (not . (== Spent)) bldr)
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

pubKeyBalance :: Plutus.PubKeyHash -> TxBuilder -> Plutus.Value
pubKeyBalance pkh =
  foldMap Plutus.txOutValue
    . Map.filter ((== Just pkh) . Plutus.txOutPubKey)
    . utxosByStatus (== Spendable)

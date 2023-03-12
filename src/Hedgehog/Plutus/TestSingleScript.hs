module Hedgehog.Plutus.TestSingleScript (
  txRunScript,
) where

import Data.Kind (Type)
import GHC.Records (HasField)

import Data.Maybe (maybeToList)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX qualified as Clock

import Lens.Micro ((^.))

import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.Data qualified as Ledger
import Cardano.Ledger.Alonzo.PParams (
  _costmdls,
  _minPoolCost,
  _poolDeposit,
  _protocolVersion,
 )
import Cardano.Ledger.Alonzo.PlutusScriptApi qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Cardano.Ledger.Babbage.PParams (
  _costmdls,
  _minPoolCost,
  _poolDeposit,
  _protocolVersion,
 )
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.Slot qualified as Ledger
import Cardano.Simple.Ledger.TimeSlot qualified as Simple
import Cardano.Slotting.EpochInfo qualified as Cardano
import Cardano.Slotting.Time qualified as Cardano
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus hiding (evaluateScriptCounting)

import Cardano.Simple.Cardano.Class qualified as Simple
import Cardano.Simple.Cardano.Common qualified as Simple
import Cardano.Simple.Ledger.Tx qualified as Simple
import Plutus.Model qualified as Model
import Plutus.Model.Mock.ProtocolParameters qualified as Model

txRunScript ::
  Model.Mock ->
  Model.Tx ->
  Plutus.ScriptPurpose ->
  Maybe Plutus.EvaluationError
txRunScript
  Model.Mock
    { Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigProtocol
        , Model.mockConfigSlotConfig =
          Simple.SlotConfig
            { Simple.scSlotLength
            , Simple.scSlotZeroTime
            }
        , Model.mockConfigNetworkId
        }
    , Model.mockUtxos
    }
  (Model.Tx extra tx)
  sp = case mockConfigProtocol of
    Model.AlonzoParams params ->
      go params
    Model.BabbageParams params ->
      go params
    where
      go ::
        forall (era :: Type).
        ( Ledger.Script era ~ Alonzo.AlonzoScript era
        , Alonzo.ExtendedUTxO era
        , Ledger.AlonzoEraTx era
        , HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer
        , HasField "_poolDeposit" (Ledger.PParams era) Ledger.Coin
        , HasField "_minPoolCost" (Ledger.PParams era) Ledger.Coin
        , HasField
            "_costmdls"
            (Ledger.PParams era)
            Alonzo.CostModels
        , Simple.IsCardanoTx era
        ) =>
        Ledger.PParams era ->
        Maybe Plutus.EvaluationError
      go params =
        txRunScript'
          params
          ( Cardano.fixedEpochInfo
              (Ledger.EpochSize 1)
              (Cardano.slotLengthFromMillisec scSlotLength)
          )
          ( Cardano.SystemStart
              . Clock.posixSecondsToUTCTime
              . fromInteger
              . (`div` 1000)
              . Plutus.getPOSIXTime
              $ scSlotZeroTime
          )
          ( unsafeFromEither $
              Simple.toUtxo
                (Simple.txScripts tx)
                mockConfigNetworkId
                (zip ins outs)
          )
          ( unsafeFromEither $
              Simple.toCardanoTx
                mockConfigNetworkId
                params
                extra
                tx
          )
          ( case sp of
              Plutus.Spending ref ->
                Ledger.Spending (unsafeFromEither $ Simple.toTxIn ref)
              Plutus.Minting sym ->
                Ledger.Minting (unsafeFromEither $ Simple.toPolicyId sym)
              Plutus.Rewarding (Plutus.StakingHash cred) ->
                Ledger.Rewarding
                  ( Ledger.RewardAcnt mockConfigNetworkId $
                      unsafeFromEither (Simple.toCredential cred)
                  )
              Plutus.Rewarding Plutus.StakingPtr {} ->
                error "StakingPtr not supported"
              Plutus.Certifying cert ->
                Ledger.Certifying
                  ( unsafeFromEither $
                      Simple.toDCert
                        mockConfigNetworkId
                        params._poolDeposit
                        params._minPoolCost
                        cert
                  )
          )

      ins :: [Simple.TxIn]
      ins =
        mconcat
          [ Set.toList $ Simple.txInputs tx
          , Set.toList $ Simple.txCollateral tx
          , Set.toList $ Simple.txReferenceInputs tx
          ]

      outs :: [Plutus.TxOut]
      outs = fmap ((mockUtxos Map.!) . Simple.txInRef) ins

txRunScript' ::
  ( Alonzo.ExtendedUTxO era
  , Ledger.AlonzoEraTx era
  , Ledger.Script era ~ Alonzo.AlonzoScript era
  , HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer
  , HasField
      "_costmdls"
      (Ledger.PParams era)
      Alonzo.CostModels
  ) =>
  Ledger.PParams era ->
  Ledger.EpochInfo (Either Text) ->
  Cardano.SystemStart ->
  Ledger.UTxO era ->
  Ledger.Tx era ->
  Ledger.ScriptPurpose (Ledger.Crypto era) ->
  Maybe Plutus.EvaluationError
txRunScript' pparams ei sysS utxo tx sp = do
  let scrs = tx ^. Ledger.witsTxL . Ledger.scriptWitsL
  sh <- sp `lookup` Alonzo.scriptsNeeded utxo tx
  (_, lang, scr') <- Alonzo.knownToNotBe1Phase scrs (sp, sh)
  rptr <- Ledger.strictMaybeToMaybe $ Ledger.rdptr (tx ^. Ledger.bodyTxL) sp
  evalScript
    lang
    pparams
    (Alonzo.unCostModels pparams._costmdls Map.! lang)
    scr'
    =<< txGetData pparams ei sysS tx utxo lang sp rptr

txGetData ::
  ( Alonzo.ExtendedUTxO era
  , Ledger.AlonzoEraTx era
  , Ledger.Script era ~ Alonzo.AlonzoScript era
  ) =>
  Ledger.PParams era ->
  Ledger.EpochInfo (Either Text) ->
  Cardano.SystemStart ->
  Ledger.Tx era ->
  Ledger.UTxO era ->
  Ledger.Language ->
  Ledger.ScriptPurpose (Ledger.Crypto era) ->
  Ledger.RdmrPtr ->
  Maybe [Ledger.Data era]
txGetData pparams ei sysS tx utxo lang sp rptr = do
  let ws = tx ^. Ledger.witsTxL
      rdmr = fst $ Ledger.unRedeemers (ws ^. Ledger.rdmrsWitsL) Map.! rptr
      dats = Ledger.unTxDats $ ws ^. Ledger.datsWitsL
  info <-
    either (const Nothing) Just $ Alonzo.txInfo pparams lang ei sysS utxo tx
  pure $ getData dats utxo info sp rdmr

getData ::
  (Alonzo.ExtendedUTxO era) =>
  Map (Ledger.DataHash (Ledger.Crypto era)) (Ledger.Data era) ->
  Ledger.UTxO era ->
  Alonzo.VersionedTxInfo ->
  Ledger.ScriptPurpose (Ledger.Crypto era) ->
  Ledger.Data era ->
  [Ledger.Data era]
getData dats utxo inf sp rdmr = datum <> [rdmr, Alonzo.valContext inf sp]
  where
    datum = maybeToList $ case sp of
      Ledger.Spending txin -> do
        txout <- Map.lookup txin (Ledger.unUTxO utxo)
        case Alonzo.getTxOutDatum txout of
          Ledger.Datum bData -> Just $ Ledger.binaryDataToData bData
          Ledger.DatumHash dh -> Map.lookup dh dats
          Ledger.NoDatum -> Nothing
      _ -> Nothing

evalScript ::
  (HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer) =>
  Ledger.Language ->
  Ledger.PParams era ->
  Ledger.CostModel ->
  Plutus.SerialisedScript ->
  [Ledger.Data era] ->
  Maybe Plutus.EvaluationError
evalScript lang pparams cm script args =
  either Just (const Nothing) . snd $
    Plutus.evaluateScriptCounting
      (toPlutusLang lang)
      (Alonzo.transProtocolVersion pparams._protocolVersion)
      Plutus.Verbose
      (Alonzo.getEvaluationContext cm)
      script
      (Ledger.getPlutusData <$> args)
  where
    toPlutusLang Ledger.PlutusV1 = Plutus.PlutusV1
    toPlutusLang Ledger.PlutusV2 = Plutus.PlutusV2

unsafeFromEither :: (Show a) => Either a b -> b
unsafeFromEither (Left a) = error $ "unsafeFromEither: " <> show a
unsafeFromEither (Right b) = b

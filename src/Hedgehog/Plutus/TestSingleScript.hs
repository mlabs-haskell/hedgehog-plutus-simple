module Hedgehog.Plutus.TestSingleScript (
  ScriptTx (ScriptTx, scriptTx, scriptTxPurpose),
  testSingleScriptBad,
  testSingleScriptGood,
  TxTest (TxTest),
  txTest,
) where

import Prelude hiding ((.))

import Data.Kind (Type)
import GHC.Records (HasField (getField))

import Control.Category ((.))
import Data.Maybe (isNothing, maybeToList)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX qualified as Clock

import Lens.Micro ((^.))

import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.Data qualified as Ledger
import Cardano.Ledger.Alonzo.PParams (_costmdls, _protocolVersion)
import Cardano.Ledger.Alonzo.PlutusScriptApi qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Cardano.Ledger.Babbage.PParams (_costmdls, _protocolVersion)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.Slot qualified as Ledger
import Cardano.Slotting.EpochInfo qualified as Cardano
import Cardano.Slotting.Time qualified as Cardano
import PlutusCore.Evaluation.Machine.Exception qualified as PLC
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus hiding (evaluateScriptCounting)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

import Plutus.Model qualified as Model
import Plutus.Model.Fork.Ledger.TimeSlot qualified as Fork
import Plutus.Model.Mock.ProtocolParameters qualified as Model

import Hedgehog ((===))
import Hedgehog qualified

import Hedgehog.Plutus.Adjunction
import Hedgehog.Plutus.TestData
import Hedgehog.Plutus.Tx

data ScriptTx = ScriptTx
  { scriptTx :: Tx 'Unbalanced
  , scriptTxPurpose :: ScriptPurpose
  }

newtype TxTest a
  = TxTest (TxContext -> Adjunction ScriptTx (Either (Bad a) (Good a)))

txTest ::
  (TestData TxContext a) =>
  (TxContext -> Adjunction ScriptTx (Generalised a)) ->
  TxTest a
txTest adj = TxTest $ \ctx -> testDataAdjunction ctx . adj ctx

txTestBad :: TxTest a -> TxContext -> Bad a -> ScriptTx
txTestBad (TxTest tt) txc = (lower . tt) txc . Left

txTestGood :: TxTest a -> TxContext -> Good a -> ScriptTx
txTestGood (TxTest tt) txc = (lower . tt) txc . Right

testSingleScriptBad ::
  (Hedgehog.MonadTest m) =>
  TxContext ->
  TxTest a ->
  Bad a ->
  m ()
testSingleScriptBad txc tt bad =
  Hedgehog.evalMaybe (txRunScript txc $ txTestBad tt txc bad) >>= \case
    Plutus.CekError ewc ->
      PLC._ewcError ewc === PLC.UserEvaluationError UPLC.CekEvaluationFailure
    _ -> Hedgehog.failure

testSingleScriptGood ::
  (Hedgehog.MonadTest m) =>
  TxContext ->
  TxTest a ->
  Good a ->
  m ()
testSingleScriptGood txc tt good =
  Hedgehog.assert $ isNothing (txRunScript txc $ txTestGood tt txc good)

txRunScript ::
  TxContext ->
  ScriptTx ->
  Maybe Plutus.EvaluationError
txRunScript
  TxContext
    { mockchain =
      Model.Mock
        { Model.mockConfig =
          Model.MockConfig
            { Model.mockConfigProtocol
            , Model.mockConfigSlotConfig =
              Fork.SlotConfig
                { Fork.scSlotLength
                , Fork.scSlotZeroTime
                }
            }
        }
    }
  ScriptTx {scriptTx, scriptTxPurpose} = case mockConfigProtocol of
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
        , HasField
            "_costmdls"
            (Ledger.PParams era)
            Alonzo.CostModels
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
          _
          _
          (toLedgerScriptPurpose scriptTxPurpose)

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
    (Alonzo.unCostModels (getField @"_costmdls" pparams) Map.! lang)
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
      (Alonzo.transProtocolVersion $ getField @"_protocolVersion" pparams)
      Plutus.Verbose
      (Alonzo.getEvaluationContext cm)
      script
      (Ledger.getPlutusData <$> args)
  where
    toPlutusLang Ledger.PlutusV1 = Plutus.PlutusV1
    toPlutusLang Ledger.PlutusV2 = Plutus.PlutusV2

{-# LANGUAGE TypeFamilyDependencies #-}
-- TODO can this be just for Prelude?
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Hedgehog.Plutus.TxTest (
  TxTest,
  txTest,
  omitted,
  resolveOmitted,
  txTestBadAdjunction,
  txTestGoodAdjunction,
  txTestBad,
  txTestGood,
) where

import Prelude hiding ((.))

import Control.Arrow (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Category (Category ((.)))

import Prelude hiding ((.))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus

import Plutus.Model qualified as Model
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..), TxInType (..))
import Hedgehog.Plutus.Adjunction (Adjunction (..), adjunctionTest)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (
  MintingPolicy (MintingPolicy),
  getValidator,
 )

import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
  ScriptTx (..),
  scriptTxValid,
 )
import Hedgehog.Plutus.TestData (
  Bad,
  --Generalised,
  Good,
  TestData,
  testDataAdjunction,
 )
import qualified Hedgehog
import PlutusLedgerApi.V1.Value qualified as Value

newtype TxTest st a
  = TxTest
      ( Model.Mock ->
        DatumOf st ->
        Adjunction (ScriptTx st) (Either (Bad a) (Good a))
      )

{- | Given an adjunction from a 'Generalised' to a 'ScriptContext', generate
a 'TxTest.

In the 'raise' direction, the supplied adjunction may omit the following
details, which will be supplied for you:

  * The 'txInfoInput' being spent, if this is a spend script

  * 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * The 'txInfoRedeemer' for the current script

  * 'txInfoData'

  * 'txInfoId'

Just pass empty lists/maps. For 'txInfoId', you can use the 'omitted' function.
-}
txTest ::
  (TestData a, Plutus.FromData r) =>
  ( Model.Mock ->
    DatumOf st ->
    Adjunction (ScriptContext r st) a
  ) ->
  TxTest st a
txTest f = TxTest $ \mock datum ->
  testDataAdjunction
    . f mock datum
    . scriptContext mock datum (error "TODO scripts table")

-- Not lawful when
-- the POSIXTimeRange doesn't coresponds to a SlotRange exactly
-- the fee contains non-ada value
scriptContext ::
  forall st r.
  Plutus.FromData r =>
  Model.Mock ->
  DatumOf st ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext m d scripts =
  Adjunction {lower = lowerSc m d scripts, raise = raiseSc m d}

lowerSc ::
  Model.Mock ->
  DatumOf st ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  ScriptContext r st ->
  ScriptTx st
lowerSc
  m@Model.Mock
    { Model.mockUsers = users
    , Model.mockUtxos = utxos
    , Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigSlotConfig = slotCfg
        }
    }
  _
  scripts
  ScriptContext
    { contextTxInfo =
      Plutus.TxInfo
        { Plutus.txInfoInputs = txInputs
        , Plutus.txInfoReferenceInputs = referenceInputs
        , Plutus.txInfoOutputs = txOutputs
        , Plutus.txInfoFee = fee
        , Plutus.txInfoMint = mint
        , Plutus.txInfoValidRange = posixRange
        , Plutus.txInfoSignatories = sigs
        , Plutus.txInfoRedeemers = redeemers
        , Plutus.txInfoData = dataTable
        }
    , contextPurpose
    , contextRedeemer = _
    } =
    ScriptTx
      { scriptTxPurpose = contextPurpose
      , scriptTx =
          Model.toExtra
          -- TODO I think it's fine to leave the extra emepty
          $
            Tx
              { txInputs = inputs
              , txReferenceInputs =
                  Set.fromList $ convertIn <$> referenceInputs
              , txOutputs = txOutputs
              , txFee = valueToAda fee
              , txMint = mint
              , txValidRange =
                  Time.posixTimeRangeToContainedSlotRange
                    slotCfg
                    posixRange
              , txSignatures =
                  Map.fromList
                    [ ( pkh
                      , maybe
                          (error "user not found")
                          Model.userSignKey
                          (Map.lookup pkh users)
                      )
                    | pkh <- sigs
                    ]
              , txRedeemers =
                  Map.fromList $
                    first (error "TODO redeemer ptr stuff")
                      <$> PlutusTx.toList redeemers
              , txData = Map.fromList $ PlutusTx.toList dataTable
              , txCollateral = collateral
              , txCollateralReturn = Just ret
              , txTotalCollateral = Just $ valueToAda totalValue
              , txScripts =
                  Map.fromList
                    [ (sh, getValidator <$> val)
                    | (sh, val) <- Map.toList scripts
                    , sh `Set.member` usedScripts
                    ]
              , txMintScripts =
                  Set.fromList
                    [ MintingPolicy . getValidator <$> val
                    | (sh, val) <- Map.toList scripts
                    , Value.CurrencySymbol (Plutus.getScriptHash sh)
                        `elem` Value.symbols mint
                    ] -- TODO make sure scripts include these
                    -- if not we need to take a table for these
              }
      }
    where
      convertIn :: Plutus.TxInInfo -> TxIn
      convertIn = convertIn' m redeemers scripts dataTable

      usedScripts :: Set Plutus.ScriptHash
      usedScripts =
        Set.fromList
          [ sh
          | txin <- Set.toList inputs
          , Plutus.ScriptCredential sh <- pure $ getCred utxos txin
          ]

      inputs :: Set TxIn
      inputs = Set.fromList $ convertIn <$> txInputs

      (collateral, totalValue, ret) = mkCollateral m inputs

getCred :: Map Plutus.TxOutRef Plutus.TxOut -> TxIn -> Plutus.Credential
getCred utxos TxIn {txInRef} = cred
  where
    Plutus.TxOut
      { Plutus.txOutAddress = Plutus.Address cred _
      } =
        fromMaybe (error "utxo lookup failed") $
          Map.lookup txInRef utxos

convertIn' ::
  Model.Mock ->
  PlutusTx.Map Plutus.ScriptPurpose Plutus.Redeemer ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  PlutusTx.Map Plutus.DatumHash Plutus.Datum ->
  Plutus.TxInInfo ->
  TxIn
convertIn'
  Model.Mock
    { Model.mockUtxos = utxos
    }
  redeemers
  scripts
  datumTable
  Plutus.TxInInfo
    { Plutus.txInInfoOutRef = txInRef
    , Plutus.txInInfoResolved =
      Plutus.TxOut
        { Plutus.txOutAddress =
          Plutus.Address
            { Plutus.addressCredential = cred
            }
        }
    } =
    TxIn {txInRef, txInType}
    where
      txInType = Just $ case cred of
        Plutus.PubKeyCredential _pkh ->
          ConsumePublicKeyAddress
        Plutus.ScriptCredential sh ->
          ConsumeScriptAddress
            ( Just $
                fromMaybe (error "script not found") $
                  Map.lookup sh scripts
            )
            ( fromMaybe (error "redeemer not found") $
                PlutusTx.lookup
                  (error "TODO convert script purpouse")
                  redeemers
            )
            ( let
                Plutus.TxOut {Plutus.txOutDatum = outDatum} =
                  fromMaybe (error "utxo lookup failed") $
                    Map.lookup txInRef utxos
               in
                getDatum datumTable outDatum
            )

getDatum :: PlutusTx.Map Plutus.DatumHash Plutus.Datum -> Plutus.OutputDatum -> Plutus.Datum
getDatum datumTable = \case
  Plutus.OutputDatum d -> d
  Plutus.OutputDatumHash dh ->
    fromMaybe (error "datum lookup failed") $
      PlutusTx.lookup dh datumTable
  Plutus.NoOutputDatum -> error "No output datum"

mkCollateral :: Model.Mock -> Set TxIn -> (Set TxIn, Plutus.Value, Plutus.TxOut)
mkCollateral
  Model.Mock
    { Model.mockUtxos = utxos
    , Model.mockUsers = users
    }
  inputs =
    (collateral, totalValue, ret)
    where
      -- Use all pubkey inputs as collateral
      collateral :: Set TxIn
      collateral =
        Set.filter
          ( \txin ->
              case getCred utxos txin of
                Plutus.PubKeyCredential _ -> True
                _ -> False
          )
          inputs

      totalValue :: Plutus.Value
      totalValue =
        mconcat
          [ val
          | TxIn {txInRef} <- Set.toList collateral
          , let
              Plutus.TxOut {Plutus.txOutValue = val} =
                fromMaybe (error "utxo lookup failed") $
                  Map.lookup txInRef utxos
          ]

      -- Return everything to the first user in the mock users
      ret :: Plutus.TxOut
      ret =
        Plutus.TxOut
          { Plutus.txOutAddress =
              Plutus.Address
                ( Plutus.PubKeyCredential
                    ( fromMaybe (error "no users") $
                        listToMaybe $
                          Map.keys users
                    )
                )
                Nothing
          , Plutus.txOutValue = totalValue
          , Plutus.txOutDatum = Plutus.NoOutputDatum
          , Plutus.txOutReferenceScript = Nothing
          }

raiseSc ::
  Plutus.FromData r =>
  Model.Mock ->
  DatumOf st ->
  ScriptTx st ->
  ScriptContext r st
raiseSc
  Model.Mock
    { Model.mockUtxos = utxos
    , Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigSlotConfig = slotCfg
        }
    }
  _
  ScriptTx
    { scriptTxPurpose
    , scriptTx =
      Model.Tx
        { Model.tx'extra = _
        , Model.tx'plutus =
          Tx
            { txInputs
            , txReferenceInputs
            , txOutputs
            , txFee
            , txMint
            , txValidRange
            , txSignatures
            , txRedeemers
            , txData
            }
        }
    } =
    ScriptContext
      { contextRedeemer =
          fromMaybe (error "failed to parse redeemer") $
            Plutus.fromBuiltinData redeemer
      , contextPurpose = scriptTxPurpose
      , contextTxInfo =
          Plutus.TxInfo
            { Plutus.txInfoInputs = convertIn <$> Set.toList txInputs
            , Plutus.txInfoReferenceInputs =
                convertIn <$> Set.toList txReferenceInputs
            , Plutus.txInfoOutputs = txOutputs
            , Plutus.txInfoFee = adaToValue txFee
            , Plutus.txInfoMint = txMint
            , Plutus.txInfoDCert = []
            , -- assume certifying nothing
              Plutus.txInfoWdrl = PlutusTx.empty
            , -- assume no staking withdrawls
              Plutus.txInfoValidRange =
                Time.slotRangeToPOSIXTimeRange slotCfg txValidRange
            , Plutus.txInfoSignatories = Map.keys txSignatures
            , Plutus.txInfoRedeemers =
                PlutusTx.fromList $
                  first (error "TODO redeemer ptr stuff")
                    <$> Map.toList txRedeemers
            , Plutus.txInfoData = PlutusTx.fromList $ Map.toList txData
            , Plutus.txInfoId =
                error "TODO"
                -- Generate the txbody and hash it?
                -- or can I just use a dummy id?
            }
      }
    where
      convertIn :: TxIn -> Plutus.TxInInfo
      convertIn TxIn {txInRef} = Plutus.TxInInfo txInRef out
        where
          out =
            fromMaybe (error "utxo lookup failure") $
              Map.lookup txInRef utxos

      redeemerPtr :: Plutus.RedeemerPtr
      redeemerPtr = error "TODO"
      -- TODO how can I get this?
      -- is this all wrong and I can get the redeemer from Extra?

      redeemer :: Plutus.BuiltinData
      redeemer =
        Plutus.getRedeemer $
          fromMaybe (error "redeemer ptr not found") $
            Map.lookup redeemerPtr txRedeemers

-- Gets the ada portion of a value
valueToAda :: Plutus.Value -> Model.Ada
valueToAda v = Model.Lovelace $ Value.valueOf v "" ""

adaToValue :: Model.Ada -> Plutus.Value
adaToValue (Model.Lovelace n) = Plutus.singleton "" "" n

omitted :: Plutus.TxId
omitted = error "You shouldn't read this"

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted _mock _d txinfo =
  txinfo
    { Plutus.txInfoInputs = []
    , Plutus.txInfoReferenceInputs = []
    , Plutus.txInfoOutputs = []
    , Plutus.txInfoData = error "TODO"
    }

txTestBadAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a), Show (ScriptTx st)
  ) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBadAdjunction (TxTest f) mock datum = adjunctionTest (f mock datum) . Left

txTestGoodAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  , Show (ScriptTx st)
  ) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Good a ->
  m ()
txTestGoodAdjunction (TxTest f) mock datum =
  adjunctionTest (f mock datum) . Right

txTestBad ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBad (TxTest f) mock datum bad =
  Hedgehog.assert $ not (scriptTxValid ((f mock datum).lower (Left bad)) mock)

txTestGood ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  Model.Mock ->
  DatumOf st ->
  Good a ->
  m ()
txTestGood (TxTest f) mock datum good =
  Hedgehog.assert $ scriptTxValid ((f mock datum).lower (Right good)) mock

{-# LANGUAGE TypeFamilyDependencies #-}
-- TODO can this be just for Prelude?
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Hedgehog.Plutus.TxTest (
  txTest,
  omitted,
  resolveOmitted,
) where

import Prelude hiding ((.))

import Control.Arrow (first)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Category (Category ((.)))

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus
import PlutusTx.AssocMap qualified

import Plutus.Model qualified as Model
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..), TxInType (..))

import Hedgehog.Plutus.Adjunction (Adjunction (..))
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
  ScriptTx (..),
 )
import Hedgehog.Plutus.TestData (
  Bad,
  Generalised,
  Good,
  TestData,
  testDataAdjunction,
 )
import PlutusLedgerApi.V1.Value (valueOf)

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

  * 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * 'txInfoRedeemers'

  * 'txInfoData'

  * 'txInfoId'

For the final three, you can use the 'omitted' function to signal this.
-}
txTest ::
  (TestData a, Plutus.FromData r) =>
  ( Model.Mock ->
    DatumOf st ->
    Adjunction (ScriptContext r st) (Generalised a)
  ) ->
  TxTest st a
txTest f = TxTest $ \mock datum ->
  testDataAdjunction
    . f mock datum
    . scriptContext mock datum

-- Not lawful when
-- the POSIXTimeRange doesn't coresponds to a SlotRange exactly
-- the fee contains non-ada value
scriptContext ::
  forall st r.
  Plutus.FromData r =>
  Model.Mock ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext
  Model.Mock
    { Model.mockUtxos = utxos
    , Model.mockUsers = users
    , Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigSlotConfig = slotCfg
        }
    }
  _ =
    Adjunction {lower, raise}
    where
      scripts :: Map Plutus.ScriptHash (Model.Versioned Model.Validator)
      scripts = error "TODO" -- probably take this map as an argument?
      lower :: ScriptContext r st -> ScriptTx st
      lower
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
          let
            toAda :: Plutus.Value -> Model.Ada
            toAda v = Model.Lovelace $ valueOf v "" ""

            convertIn :: Plutus.TxInInfo -> TxIn
            convertIn
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
                            case outDatum of
                              Plutus.OutputDatum d -> d
                              Plutus.OutputDatumHash dh ->
                                fromMaybe (error "datum lookup failed") $
                                  PlutusTx.lookup dh dataTable
                              Plutus.NoOutputDatum -> error "No output datum"
                        )
            inputs :: Set TxIn
            inputs = Set.fromList $ convertIn <$> txInputs

            -- Use all pubkey inputs as collateral
            collateral :: Set TxIn
            collateral =
              Set.filter
                ( \TxIn {txInRef} ->
                    let
                      Plutus.TxOut
                        { Plutus.txOutAddress =
                          Plutus.Address cred _
                        } =
                          fromMaybe (error "utxo lookup failed") $
                            Map.lookup txInRef utxos
                     in
                      case cred of
                        Plutus.PubKeyCredential _ -> True
                        _ -> False
                )
                inputs

            totalCollateralValue :: Plutus.Value
            totalCollateralValue =
              mconcat
                [ val
                | TxIn {txInRef} <- Set.toList collateral
                , let
                    Plutus.TxOut {Plutus.txOutValue = val} =
                      fromMaybe (error "utxo lookup failed") $
                        Map.lookup txInRef utxos
                ]

            -- Return everything to the first user in the mock users
            collateralRet :: Plutus.TxOut
            collateralRet =
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
                , Plutus.txOutValue = totalCollateralValue
                , Plutus.txOutDatum = Plutus.NoOutputDatum
                , Plutus.txOutReferenceScript = Nothing
                }
           in
            ScriptTx
              { scriptTxPurpose = contextPurpose
              , scriptTx =
                  Model.Tx
                    (error "TODO extra")
                    $ Tx
                      { txInputs = inputs
                      , txReferenceInputs =
                          Set.fromList $ convertIn <$> referenceInputs
                      , txOutputs = txOutputs
                      , txFee = toAda fee
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
                      , txCollateralReturn = Just collateralRet
                      , txTotalCollateral = Just $ toAda totalCollateralValue
                      , txScripts = error "TODO"
                      , -- TODO if we add the scripts argument this
                        -- can be a filter of that
                        txMintScripts = error "TODO"
                        -- TODO do we need a table to look these up
                        -- from currencySybmols
                      }
              }
      raise :: ScriptTx st -> ScriptContext r st
      raise
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
          let
            convertIn :: TxIn -> Plutus.TxInInfo
            convertIn TxIn {txInRef} = Plutus.TxInInfo txInRef out
              where
                out =
                  fromMaybe (error "lookup failure") $
                    Map.lookup txInRef utxos

            adaToValue :: Model.Ada -> Plutus.Value
            adaToValue (Model.Lovelace n) = Plutus.singleton "" "" n

            redeemerPtr :: Plutus.RedeemerPtr
            redeemerPtr = error "TODO"
            -- TODO how can I get this?
            -- is this all wrong and I can get the redeemer from Extra?

            redeemer :: Plutus.Redeemer
            redeemer =
              fromMaybe (error "redeemer ptr not found") $
                Map.lookup redeemerPtr txRedeemers

            redeemerData :: Plutus.BuiltinData
            redeemerData = case redeemer of
              Plutus.Redeemer d -> d
           in
            ScriptContext
              { contextRedeemer =
                  fromMaybe (error "failed to parse redeemer") $
                    Plutus.fromBuiltinData redeemerData
              , contextPurpose = scriptTxPurpose
              , contextTxInfo =
                  Plutus.TxInfo
                    { Plutus.txInfoInputs = convertIn <$> Set.toList txInputs
                    , Plutus.txInfoReferenceInputs =
                        convertIn <$> Set.toList txReferenceInputs
                    , Plutus.txInfoOutputs = txOutputs
                    , Plutus.txInfoFee = adaToValue txFee
                    , Plutus.txInfoMint = txMint
                    , Plutus.txInfoDCert = error "TODO"
                    , Plutus.txInfoWdrl = error "TODO"
                    , Plutus.txInfoValidRange =
                        Time.slotRangeToPOSIXTimeRange slotCfg txValidRange
                    , Plutus.txInfoSignatories = Map.keys txSignatures
                    , Plutus.txInfoRedeemers =
                        PlutusTx.fromList $
                          first (error "TODO redeemer ptr stuff")
                            <$> Map.toList txRedeemers
                    , Plutus.txInfoData = PlutusTx.fromList $ Map.toList txData
                    , Plutus.txInfoId = error "TODO"
                    }
              }

type Omittable :: Type -> Constraint
class Omittable a
instance Omittable (PlutusTx.AssocMap.Map Plutus.ScriptPurpose Plutus.Redeemer)
instance Omittable (PlutusTx.AssocMap.Map Plutus.DatumHash Plutus.Datum)
instance Omittable Plutus.TxId

omitted :: (Omittable a) => a
omitted = error "ommited value read"

resolveOmitted :: Model.Mock -> datum -> Plutus.TxInfo -> Plutus.TxInfo
resolveOmitted _mock _d txinfo =
  txinfo
    { Plutus.txInfoInputs = []
    , Plutus.txInfoReferenceInputs = []
    , Plutus.txInfoOutputs = []
    , Plutus.txInfoData = error "TODO"
    }

-- txTestRight ::
--   forall (ingrs :: Type).
--   TxTest ingrs ->
--   Model.Tx ->
--   Either (Bad ingrs) ingrs
-- txTestRight (TxTest Adjunction {raise}) = raise

-- txTestTestBad ::
--   forall (m :: Type -> Type) (ingrs :: Type).
--   ( Hedgehog.MonadTest m
--   , Eq ingrs
--   , Show ingrs
--   , Eq (Bad ingrs)
--   , Show (Bad ingrs)
--   ) =>
--   TxTest ingrs ->
--   Bad ingrs ->
--   m ()
-- txTestTestBad tt bad = txTestRight tt (txTestBad tt bad) === Left bad

-- txTestTestGood ::
--   forall (m :: Type -> Type) (ingrs :: Type).
--   ( Hedgehog.MonadTest m
--   , Eq ingrs
--   , Show ingrs
--   , Eq (Bad ingrs)
--   , Show (Bad ingrs)
--   ) =>
--   TxTest ingrs ->
--   ingrs ->
--   m ()
-- txTestTestGood tt good = txTestRight tt (txTestGood tt good) === Right good

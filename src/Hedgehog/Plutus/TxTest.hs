{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest (
  TxTest (TxTest),
  ChainState (..),
  txTest,
  omitted,
  txTestBadAdjunction,
  txTestGoodAdjunction,
  txTestBad,
  txTestGood,
  resolveOmitted,
) where

import Control.Arrow (first)
import Data.List (find, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import PlutusLedgerApi.V1.Value qualified as Value

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus

import Plutus.Model qualified as Model
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Cardano.Class (IsCardanoTx (getTxBody, toCardanoTx))
import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..), TxInType (..))
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (getValidator)
import Cardano.Simple.TxExtra (Extra, Mint (Mint), extra'mints)

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Crypto (StandardCrypto)

import Cardano.Ledger.Block qualified as Ledger
import Cardano.Ledger.Core (EraTxBody)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.TxIn qualified as Ledger

import Hedgehog.Plutus.Adjunction (Adjunction (..), adjunctionTest)
import Hedgehog.Plutus.ScriptContext (
  DatumOf,
  ScriptContext (..),
  ScriptPurpose (..),
  ScriptTx (..),
  scriptTxValid,
 )
import Hedgehog.Plutus.TestData (
  Bad,
  Good,
  TestData,
  testDataAdjunction,
 )
import Plutus.Model qualified as Modele
import Plutus.Model.Mock.ProtocolParameters (PParams (..))

import Hedgehog qualified

newtype TxTest st a
  = TxTest
      ( ChainState ->
        DatumOf st ->
        Adjunction (ScriptTx st) (Either (Bad a) (Good a))
      )

data ChainState = ChainState
  { csMock :: Model.Mock
  , csScripts :: Map Plutus.ScriptHash (Model.Versioned Model.Validator)
  , csMps :: Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy)
  }

{- | Given an adjunction from a 'Generalised' to a 'ScriptContext', generate
a 'TxTest.

In the 'raise' direction, the supplied adjunction may omit the following
details, which will be supplied for you:

  * any 'txInfoInputs` impiled by the script purpose

  * any 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * 'txInfoRedeemers'

  * 'txInfoData'

  * 'txInfoId'

Just pass empty lists/maps. For 'txInfoId', you can use the 'omitted' function.
-}
txTest ::
  (TestData a, Plutus.FromData r) =>
  ( ChainState ->
    DatumOf st ->
    Adjunction (ScriptContext r st) a
  ) ->
  TxTest st a
txTest f = TxTest $ \cs datum ->
  testDataAdjunction
    . f cs datum
    . scriptContext cs datum

-- Not lawful when
-- the POSIXTimeRange doesn't coresponds to a SlotRange exactly
-- the fee contains non-ada value
scriptContext ::
  forall st r.
  Plutus.FromData r =>
  ChainState ->
  DatumOf st ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext
  ChainState
    { csMock = m
    , csScripts = scripts
    , csMps = mps
    }
  d =
    Adjunction {lower = lowerSc m d scripts mps, raise = raiseSc m d scripts}

lowerSc ::
  forall st r.
  Model.Mock ->
  DatumOf st ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  ScriptContext r st ->
  ScriptTx st
lowerSc
  m
  d
  scripts
  mps
  ScriptContext
    { contextTxInfo
    , contextPurpose
    } =
    ScriptTx
      { scriptTxPurpose = contextPurpose
      , scriptTx = lowerScCore @st m d scripts mps contextTxInfo
      }

revLookup ::
  Plutus.ScriptPurpose ->
  Map Plutus.RedeemerPtr Plutus.ScriptPurpose ->
  Maybe Plutus.RedeemerPtr
revLookup sp m = fst <$> find ((== sp) . snd) (Map.toList m)

-- this O(n) lookup is not great
-- There's no (Ord Plutus.ScriptPurpose)
-- maybe a hashmap would help if this proves
-- to be a problem

getTxId ::
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Model.Mock ->
  Tx ->
  Extra ->
  Plutus.TxId
getTxId
  scripts'
  Modele.Mock
    { Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigNetworkId = network
        , Model.mockConfigProtocol = params'
        }
    }
  tx
  extra =
    Plutus.TxId . Plutus.toBuiltin . SafeHash.originalBytes . Ledger._unTxId $
      case params' of
        AlonzoParams params -> go @(AlonzoEra StandardCrypto) params
        BabbageParams params -> go @(BabbageEra StandardCrypto) params
    where
      go ::
        forall era.
        (IsCardanoTx era, EraTxBody era) =>
        Core.PParams era ->
        Ledger.TxId (Core.Crypto era)
      go params = Ledger.txid @era . getTxBody $
        case toCardanoTx @era scripts network params extra tx of
          Left err -> error $ "toCardanoTx failed with: " <> err
          Right ctx -> ctx
      scripts = Map.map (fmap getValidator) scripts'

getCred :: Map Plutus.TxOutRef Plutus.TxOut -> TxIn -> Plutus.Credential
getCred utxos TxIn {txInRef} = cred
  where
    Plutus.TxOut
      { Plutus.txOutAddress = Plutus.Address cred _
      } =
        fromMaybe (error "utxo lookup failed") $
          Map.lookup txInRef utxos

mkRedMap ::
  PlutusTx.Map Plutus.StakingCredential Integer ->
  [Plutus.DCert] ->
  Plutus.Value ->
  [TxIn] ->
  Map Plutus.RedeemerPtr Plutus.ScriptPurpose
mkRedMap stake cert mint inputs = mconcat [spends, mints, certs, stakes]
  where
    spends =
      Map.fromList
        [ ( Plutus.RedeemerPtr Plutus.Spend i
          , Plutus.Spending ref
          )
        | (i, ref) <- sortAndLabel (txInRef <$> inputs)
        ]
    mints =
      Map.fromList
        [ ( Plutus.RedeemerPtr Plutus.Mint i
          , Plutus.Minting cs
          )
        | (i, cs) <- sortAndLabel $ Value.symbols mint
        ]
    certs =
      Map.fromList
        [ ( Plutus.RedeemerPtr Plutus.Cert i
          , Plutus.Certifying dcert
          )
        | (i, dcert) <- sortAndLabel cert
        ]
    stakes =
      Map.fromList
        [ ( Plutus.RedeemerPtr Plutus.Reward i
          , Plutus.Rewarding sc
          )
        | (i, sc) <- sortAndLabel $ fst <$> PlutusTx.toList stake
        ]

sortAndLabel :: Ord a => [a] -> [(Integer, a)]
sortAndLabel = zip [0 ..] . sort

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
                  (Plutus.Spending txInRef)
                  redeemers
            )
            ( let
                Plutus.TxOut {Plutus.txOutDatum = outDatum} =
                  fromMaybe (error "utxo lookup failed") $
                    Map.lookup txInRef utxos
               in
                getDatum datumTable outDatum
            )

getDatum ::
  PlutusTx.Map Plutus.DatumHash Plutus.Datum ->
  Plutus.OutputDatum ->
  Plutus.Datum
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
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  ScriptTx st ->
  ScriptContext r st
raiseSc
  mock@Model.Mock
    { Model.mockUtxos = utxos
    , Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigSlotConfig = slotCfg
        }
    }
  _
  scripts
  ScriptTx
    { scriptTxPurpose
    , scriptTx =
      Model.Tx
        { Model.tx'extra = extra
        , Model.tx'plutus =
          tx@Tx
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
                  first
                    ( fromMaybe (error "redeemer ptr not found")
                        . (`Map.lookup` redMap)
                    )
                    <$> Map.toList txRedeemers
            , Plutus.txInfoData = PlutusTx.fromList $ Map.toList txData
            , Plutus.txInfoId = getTxId scripts mock tx extra
            }
      }
    where
      convertIn :: TxIn -> Plutus.TxInInfo
      convertIn TxIn {txInRef} = Plutus.TxInInfo txInRef out
        where
          out =
            fromMaybe (error "utxo lookup failure") $
              Map.lookup txInRef utxos

      redMap :: Map Plutus.RedeemerPtr Plutus.ScriptPurpose
      redMap = mkRedMap PlutusTx.empty [] txMint (Set.toList txInputs)

      redeemerPtr :: Plutus.RedeemerPtr
      redeemerPtr =
        fromMaybe (error "script purpose not found") $
          revLookup sp redMap

      sp :: Plutus.ScriptPurpose
      sp = case scriptTxPurpose of
        Spending ref -> Plutus.Spending ref
        Minting cs -> Plutus.Minting cs
        Rewarding sc -> Plutus.Rewarding sc
        Certifying dc -> Plutus.Certifying dc

      redeemer :: Plutus.BuiltinData
      redeemer =
        Plutus.getRedeemer $
          fromMaybe (error "redeemer ptr not found") $
            Map.lookup redeemerPtr txRedeemers

lowerScCore ::
  forall st.
  Model.Mock ->
  DatumOf st ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  Plutus.TxInfo ->
  Model.Tx
lowerScCore
  m@Model.Mock
    { Model.mockUsers = users
    , Model.mockUtxos = utxos
    , Model.mockConfig =
      Model.MockConfig
        { Model.mockConfigSlotConfig = slotCfg
        }
    }
  d
  scripts
  mps
  ( resolveOmitted @st m scripts mps d ->
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
        , Plutus.txInfoDCert = dcerts
        , Plutus.txInfoWdrl = stakes
        }
    ) =
    Model.Tx
      mempty
        { extra'mints =
            [ Mint val red mp
            | (cs, m) <- PlutusTx.toList $ Value.getValue mint
            , let val =
                    mconcat
                      [ Value.singleton cs tn n
                      | (tn, n) <- PlutusTx.toList m
                      ]
            , let mp =
                    fromMaybe (error "mp not found") $
                      Map.lookup cs mps
            , let red =
                    fromMaybe (error "redeemer not found") $
                      PlutusTx.lookup (Plutus.Minting cs) redeemers
            ]
        }
      $ Tx
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
              first getPtr
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
        , txMintScripts = Set.empty
        -- this will be computed from Extra later
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
      inputs = Set.fromList inputs'

      inputs' :: [TxIn]
      inputs' = convertIn <$> txInputs

      getPtr :: Plutus.ScriptPurpose -> Plutus.RedeemerPtr
      getPtr sp =
        fromMaybe (error "script purpose not found") $
          revLookup sp redMap

      redMap :: Map Plutus.RedeemerPtr Plutus.ScriptPurpose
      redMap = mkRedMap stakes dcerts mint inputs'

      collateral :: Set TxIn
      totalValue :: Plutus.Value
      ret :: Plutus.TxOut
      (collateral, totalValue, ret) = mkCollateral m inputs

-- Gets the ada portion of a value
valueToAda :: Plutus.Value -> Model.Ada
valueToAda v = Model.Lovelace $ Value.valueOf v "" ""

adaToValue :: Model.Ada -> Plutus.Value
adaToValue (Model.Lovelace n) = Plutus.singleton "" "" n

omitted :: Plutus.TxId
omitted = error "You shouldn't read this"

resolveOmitted ::
  forall st.
  Model.Mock ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  DatumOf st ->
  Plutus.ScriptPurpose ->
  Plutus.TxInfo ->
  Plutus.TxInfo
resolveOmitted
  mock@Model.Mock {Model.mockUtxos = utxos}
  scripts
  mps
  d
  sp
  txinfo =
    resolved
    where
      resolved =
        txinfo
          { Plutus.txInfoId = getTxId scripts mock tx extra
          , Plutus.txInfoInputs = inputs'
          , Plutus.txInfoSignatories =
              Set.toList . Set.fromList $
                [ pkh
                | Plutus.PubKeyCredential pkh <-
                    getCred utxos <$> inputs
                ]
                  <> Plutus.txInfoSignatories txinfo
          , Plutus.txInfoRedeemers = redeemers
          , Plutus.txInfoData = datums
          }
      Model.Tx extra tx = lowerScCore @st mock d scripts mps resolved
      -- This should always halt because
      -- extra and tx are only used in the txInfoId
      -- an lowerScCore doesn't look at that feild
      inputs :: [TxIn]
      inputs =
        convertIn' mock redeemers scripts datums
          <$> inputs'
      inputs' :: [Plutus.TxInInfo]
      inputs' = Plutus.txInfoInputs txinfo <> extraInputs
      extraInputs =
        case sp of
          Plutus.Spending ref ->
            pure
              Plutus.TxInInfo
                { Plutus.txInInfoOutRef = ref
                , Plutus.txInInfoResolved =
                    fromMaybe (error "utxo lookup failed") $
                      Map.lookup ref utxos
                }
          _ -> []
      datums =
        PlutusTx.fromList
          [ ( dh
            , fromMaybe (error "datum hash not found") $
                Map.lookup dh (Model.mockDatums mock)
            )
          | TxIn {txInRef} <- inputs
          , let Plutus.TxOut {Plutus.txOutDatum = out} =
                  fromMaybe (error "utxo lookup failure") $
                    Map.lookup txInRef utxos
          , Plutus.OutputDatumHash dh <- pure out
          ]
      redeemers = error "TODO where do I get this?"

txTestBadAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  ) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBadAdjunction (TxTest f) cs datum =
  adjunctionTest (f cs datum) . Left

txTestGoodAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  ) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Good a ->
  m ()
txTestGoodAdjunction (TxTest f) cs datum =
  adjunctionTest (f cs datum) . Right

txTestBad ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBad (TxTest f) cs datum bad =
  Hedgehog.assert $
    not
      ( scriptTxValid
          ((f cs datum).lower (Left bad))
          (cs.csMock)
      )

txTestGood ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Good a ->
  m ()
txTestGood (TxTest f) cs datum good =
  Hedgehog.assert $ scriptTxValid ((f cs datum).lower (Right good)) (cs.csMock)

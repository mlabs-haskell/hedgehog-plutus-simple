{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hedgehog.Plutus.TxTest (
  TxTest (TxTest),
  ChainState (ChainState, csMock, csScripts, csMps),
  txTest,
  omitted,
  resolveOmitted,
  txTestBadAdjunction,
  txTestGoodAdjunction,
  txTestBad,
  txTestGood,
  ppChainState,
) where

import Control.Arrow (Arrow (second), first)
import Data.List (find, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter (Pretty, pretty, vcat)

import Plutus.Model qualified as Model
import Plutus.Model qualified as Modele
import Plutus.Model.Mock.ProtocolParameters (PParams (..))
import Plutus.Model.V1 qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V2.Tx qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusTx

import Cardano.Simple.Cardano.Class (IsCardanoTx (getTxBody, toCardanoTx))
import Cardano.Simple.Ledger.TimeSlot qualified as Time
import Cardano.Simple.Ledger.Tx (Tx (..), TxIn (..), TxInType (..))
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (getValidator)
import Cardano.Simple.TxExtra (Extra, Mint (Mint), extra'mints)

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)

import Cardano.Ledger.Block qualified as Ledger
import Cardano.Ledger.Core (EraTxBody)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.TxIn qualified as Ledger

import Hedgehog qualified
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

ppChainState :: ChainState -> String
ppChainState = show . pretty

instance Pretty ChainState where
  pretty (ChainState mock scripts mps) =
    vcat
      [ "mock    : " <> pretty mock
      , "scripts : " <> pretty (second show <$> Map.toList scripts)
      , "steps   : " <> pretty (second show <$> Map.toList mps)
      ]

{- | Given an adjunction from a 'Generalised' to a 'ScriptContext', generate
a 'TxTest.

In the 'raise' direction, the supplied adjunction may omit the following
details, which will be supplied for you:

  * any 'txInfoInputs` impiled by the script purpose

  * any 'txInfoSignatories' corresponding to 'PubKeyHash' inputs

  * The 'txInfoRedeemer' for the current script

  * 'txInfoData'

  * 'txInfoId'

Just pass empty lists/maps. For 'txInfoId', you can use the 'omitted' function.
-}
txTest ::
  forall r a st.
  (TestData a, Plutus.IsData r) =>
  ( ChainState ->
    DatumOf st ->
    Adjunction (ScriptContext r st) a
  ) ->
  TxTest st a
txTest f = TxTest $ \cs datum ->
  testDataAdjunction
    . f cs datum
    . scriptContext @r cs

-- Not lawful when
-- the POSIXTimeRange doesn't coresponds to a SlotRange exactly
-- the fee contains non-ada value
scriptContext ::
  forall r st.
  Plutus.IsData r =>
  ChainState ->
  Adjunction (ScriptTx st) (ScriptContext r st)
scriptContext
  ChainState
    { csMock = m
    , csScripts = scripts
    , csMps = mps
    } =
    Adjunction {lower = lowerSc m scripts mps, raise = raiseSc m scripts}

lowerSc ::
  forall r st.
  Plutus.ToData r =>
  Model.Mock ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  ScriptContext r st ->
  ScriptTx st
lowerSc
  m
  scripts
  mps
  ScriptContext
    { contextTxInfo
    , contextPurpose
    } =
    ScriptTx
      { scriptTxPurpose = contextPurpose
      , scriptTx = lowerScCore @r m scripts mps sp contextTxInfo
      }
    where
      sp = toPlutusSp contextPurpose

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

convertInInfo' ::
  Model.Mock ->
  PlutusTx.Map Plutus.ScriptPurpose Plutus.Redeemer ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  PlutusTx.Map Plutus.DatumHash Plutus.Datum ->
  Plutus.TxInInfo ->
  TxIn
convertInInfo'
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

raiseSc ::
  Plutus.FromData r =>
  Model.Mock ->
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
      sp = toPlutusSp scriptTxPurpose

      redeemer :: Plutus.BuiltinData
      redeemer =
        Plutus.getRedeemer $
          fromMaybe (error "redeemer ptr not found") $
            Map.lookup redeemerPtr txRedeemers

toPlutusSp :: ScriptPurpose st -> Plutus.ScriptPurpose
toPlutusSp = \case
  Spending ref -> Plutus.Spending ref
  Minting cs -> Plutus.Minting cs
  Rewarding sc -> Plutus.Rewarding sc
  Certifying dc -> Plutus.Certifying dc

lowerScCore ::
  forall r.
  Plutus.ToData r =>
  Model.Mock ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  Plutus.ScriptPurpose ->
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
  scripts
  mps
  sp
  ( resolveOmitted @r m scripts mps sp ->
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
            Set.fromList $ convertInInfo <$> referenceInputs
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
        , txCollateral = Set.empty
        , txCollateralReturn = Nothing
        , txTotalCollateral = Nothing
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
      convertInInfo :: Plutus.TxInInfo -> TxIn
      convertInInfo = convertInInfo' m redeemers scripts dataTable

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
      inputs' = convertInInfo <$> txInputs

      getPtr :: Plutus.ScriptPurpose -> Plutus.RedeemerPtr
      getPtr sp =
        fromMaybe (error "script purpose not found") $
          revLookup sp redMap

      redMap :: Map Plutus.RedeemerPtr Plutus.ScriptPurpose
      redMap = mkRedMap stakes dcerts mint inputs'

-- Gets the ada portion of a value
valueToAda :: Plutus.Value -> Model.Ada
valueToAda v = Model.Lovelace $ Value.valueOf v "" ""

adaToValue :: Model.Ada -> Plutus.Value
adaToValue (Model.Lovelace n) = Plutus.singleton "" "" n

omitted :: Plutus.TxId
omitted = error "You shouldn't read this"

resolveOmitted ::
  forall r.
  Plutus.ToData r =>
  Model.Mock ->
  Map Plutus.ScriptHash (Model.Versioned Model.Validator) ->
  Map Plutus.CurrencySymbol (Model.Versioned Model.MintingPolicy) ->
  Plutus.ScriptPurpose ->
  Plutus.TxInfo ->
  Plutus.TxInfo
resolveOmitted
  mock@Model.Mock {Model.mockUtxos = utxos}
  scripts
  mps
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
      Model.Tx extra tx = lowerScCore @r mock scripts mps sp resolved
      -- This should always halt because
      -- extra and tx are only used in the txInfoId
      -- an lowerScCore doesn't look at that field
      inputs :: [TxIn]
      inputs =
        convertInInfo' mock redeemers scripts datums
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
  , Show (ScriptTx st)
  ) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBadAdjunction (TxTest f) cs datum = adjunctionTest (f cs datum) . Left

txTestGoodAdjunction ::
  ( Hedgehog.MonadTest m
  , Eq (Bad a)
  , Eq (Good a)
  , Show (Bad a)
  , Show (Good a)
  , Show (ScriptTx st)
  ) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Good a ->
  m ()
txTestGoodAdjunction (TxTest f) mock datum =
  adjunctionTest (f mock datum) . Right

txTestBad ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Bad a ->
  m ()
txTestBad (TxTest f) cs datum bad =
  Hedgehog.assert $ not (scriptTxValid ((f cs datum).lower (Left bad)) cs.csMock)

txTestGood ::
  (Hedgehog.MonadTest m) =>
  TxTest st a ->
  ChainState ->
  DatumOf st ->
  Good a ->
  m ()
txTestGood (TxTest f) cs datum good =
  Hedgehog.assert $ scriptTxValid ((f cs datum).lower (Right good)) cs.csMock

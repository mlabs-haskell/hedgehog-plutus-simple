module Time where

import Hedgehog (Gen, Group (Group), Property, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cardano.Simple.Ledger.Slot (Slot, SlotRange)
import Cardano.Simple.Ledger.TimeSlot (
  SlotConfig (SlotConfig),
  posixTimeRangeToContainedSlotRange,
  slotRangeToPOSIXTimeRange,
 )
import PlutusLedgerApi.V1 (POSIXTimeRange)
import PlutusLedgerApi.V2 (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )

import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction), adjunctionTest)

timeTests :: Group
timeTests =
  Group
    "time adjunction"
    [ ("from POSIX", fromPosix)
    , ("from Slot", fromSlot)
    ]

fromPosix :: Property
fromPosix = property $ do
  slotCfg <- forAll genSlotCfg
  timeRange <- forAll $ genPOSIXRange slotCfg
  adjunctionTest (timeAdjunctionPosix slotCfg) timeRange

fromSlot :: Property
fromSlot = property $ do
  slotCfg <- forAll genSlotCfg
  slotRange <- forAll genSlotRange
  adjunctionTest (timeAdjunctionSlot slotCfg) slotRange

-- generates a POSIXTimeRange which coresponds to a SlotRange exactly
genPOSIXRange :: SlotConfig -> Gen POSIXTimeRange
genPOSIXRange sc = slotRangeToPOSIXTimeRange sc <$> genSlotRange

genSlotRange :: Gen SlotRange
genSlotRange = do
  a <- genExtended genSlot
  b <- Gen.filter (/= a) $ genExtended genSlot
  let l = min a b
  let r = max a b
  Interval
    <$> (LowerBound l <$> Gen.bool)
    <*> (UpperBound r <$> Gen.bool)

genSlot :: Gen Slot
genSlot = Gen.integral $ Range.linear 0 1_000_000

genExtended :: Gen a -> Gen (Extended a)
genExtended gen =
  Gen.choice
    [ pure NegInf
    , pure PosInf
    , Finite <$> gen
    ]

genSlotCfg :: Gen SlotConfig
genSlotCfg = do
  a <- Gen.integral $ Range.linear 1 1_000_000
  b <- Gen.integral $ Range.linear 0 1_000_000
  pure $ SlotConfig a b

-- Only lawful if the POSIXTimeRange coresponds to a SlotRange exactly
timeAdjunctionPosix :: SlotConfig -> Adjunction SlotRange POSIXTimeRange
timeAdjunctionPosix slotCfg =
  Adjunction
    (posixTimeRangeToContainedSlotRange slotCfg)
    (slotRangeToPOSIXTimeRange slotCfg)

-- Lawful
timeAdjunctionSlot :: SlotConfig -> Adjunction POSIXTimeRange SlotRange
timeAdjunctionSlot slotCfg =
  Adjunction
    (slotRangeToPOSIXTimeRange slotCfg)
    (posixTimeRangeToContainedSlotRange slotCfg)

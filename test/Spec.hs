import Hedgehog (Gen, Group (Group), Property, checkParallel, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main (defaultMain)
import Hedgehog.Range qualified as Range

import Hedgehog.Plutus.Adjunction (adjunctionTest)

import Cardano.Simple.Ledger.Slot (Slot, SlotRange)
import Cardano.Simple.Ledger.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Hedgehog.Plutus.TimeTest (timeAdjunctionPosix, timeAdjunctionSlot)
import PlutusLedgerApi.V1 (POSIXTime, POSIXTimeRange)
import PlutusLedgerApi.V2 (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )

main :: IO ()
main =
  defaultMain $
    [ checkParallel $
        Group
          "time adjunction"
          [ ("from POSIX", fromPosix)
          , ("from Slot", fromSlot)
          ]
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

genPOSIXRange :: SlotConfig -> Gen POSIXTimeRange
genPOSIXRange sc = genInterval (genPOSIXTime sc)

genSlotRange :: Gen SlotRange
genSlotRange = genInterval genSlot

genInterval :: Ord a => Gen a -> Gen (Interval a)
genInterval g = do
  a <- genExtended g
  b <- Gen.filter (/= a) $ genExtended g
  let l = min a b
  let r = max a b
  cl <- Gen.bool
  cr <- Gen.bool
  pure $ Interval (LowerBound l cl) (UpperBound r cr)

genSlot :: Gen Slot
genSlot = Gen.integral $ Range.linear 0 1_000_000

genExtended :: Gen a -> Gen (Extended a)
genExtended gen =
  Gen.choice
    [ pure NegInf
    , pure PosInf
    , Finite <$> gen
    ]

genPOSIXTime :: SlotConfig -> Gen POSIXTime
genPOSIXTime SlotConfig {scSlotLength, scSlotZeroTime} = do
  n <- Gen.int $ Range.linear 0 1_000_000
  pure $ scSlotZeroTime + fromIntegral (n * fromIntegral scSlotLength)

genSlotCfg :: Gen SlotConfig
genSlotCfg = do
  a <- Gen.integral $ Range.linear 1 1_000_000
  b <- Gen.integral $ Range.linear 0 1_000_000
  pure $ SlotConfig a b

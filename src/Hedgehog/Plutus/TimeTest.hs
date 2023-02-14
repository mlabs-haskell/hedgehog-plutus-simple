module Hedgehog.Plutus.TimeTest (
  timeAdjunctionSlot,
  timeAdjunctionPosix,
) where

import Cardano.Simple.Ledger.Slot (SlotRange)
import Cardano.Simple.Ledger.TimeSlot (
  SlotConfig,
  posixTimeRangeToContainedSlotRange,
  slotRangeToPOSIXTimeRange,
 )
import Hedgehog.Plutus.Adjunction (Adjunction (Adjunction))
import PlutusLedgerApi.V1 (POSIXTimeRange)

timeAdjunctionPosix :: SlotConfig -> Adjunction SlotRange POSIXTimeRange
timeAdjunctionPosix slotCfg =
  Adjunction
    (posixTimeRangeToContainedSlotRange slotCfg)
    (slotRangeToPOSIXTimeRange slotCfg)

timeAdjunctionSlot :: SlotConfig -> Adjunction POSIXTimeRange SlotRange
timeAdjunctionSlot slotCfg =
  Adjunction
    (slotRangeToPOSIXTimeRange slotCfg)
    (posixTimeRangeToContainedSlotRange slotCfg)

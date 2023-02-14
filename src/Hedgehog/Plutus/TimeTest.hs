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

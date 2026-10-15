-- | Gravity follows elapsed time rather than adding event handling to each tick.
module UI.GravityClock
  ( GravityClock
  , newGravityClock
  , gravityDeadline
  , advanceGravityClock
  ) where

import Data.Word (Word64)

data GravityClock = GravityClock !Word64 !Word64 deriving (Eq, Show)

newGravityClock :: Int -> Word64 -> GravityClock
newGravityClock micros now = GravityClock period (now + period)
  where period = fromIntegral (max 1 micros) * 1000

gravityDeadline :: GravityClock -> Word64
gravityDeadline (GravityClock _ deadline) = deadline

-- | Keep the previous phase during ordinary work. Skip missed slots after a
-- stall so resuming never produces a burst of queued falling steps. A level
-- change starts a fresh interval at the new speed.
advanceGravityClock :: Int -> Word64 -> GravityClock -> GravityClock
advanceGravityClock micros now clock@(GravityClock oldPeriod deadline)
  | period /= oldPeriod = newGravityClock micros now
  | now < deadline = clock
  | otherwise = GravityClock period (deadline + ((now - deadline) `div` period + 1) * period)
  where period = fromIntegral (max 1 micros) * 1000

-- | A monotonic frame budget, independent of the simulation's gravity timer.
module UI.FrameClock
  ( FrameClock
  , newFrameClock
  , frameDelayMicros
  , advanceFrameClock
  ) where

import Data.Word (Word64)

data FrameClock = FrameClock !Word64 !Word64 deriving (Eq, Show)

-- | Zero disables pacing. The first screen is drawn immediately by Brick;
-- subsequent changes share the next frame budget.
newFrameClock :: Int -> Word64 -> FrameClock
newFrameClock fps now
  | fps <= 0 = FrameClock 0 now
  | otherwise = FrameClock period (now + period)
  where period = (1000000000 + fromIntegral fps - 1) `div` fromIntegral fps

frameDelayMicros :: Word64 -> FrameClock -> Int
frameDelayMicros now (FrameClock period deadline)
  | period == 0 || now >= deadline = 0
  | otherwise = fromIntegral $ (deadline - now + 999) `div` 1000

-- | Keep the clock's phase, skipping expired budgets after a stall. One render
-- advances directly to a future deadline, without a burst of catch-up frames.
advanceFrameClock :: Word64 -> FrameClock -> FrameClock
advanceFrameClock now clock@(FrameClock period deadline)
  | period == 0 = FrameClock 0 now
  | now < deadline = clock
  | next - now < period `div` 2 = FrameClock period (next + period)
  | otherwise = FrameClock period next
  where next = deadline + ((now - deadline) `div` period + 1) * period

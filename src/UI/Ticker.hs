-- | A one-shot gravity timer. Acknowledging a tick schedules the next one, so
-- a slow terminal never accumulates a backlog of gravity events.
module UI.Ticker
  ( Tick(..)
  , Ticker
  , newTicker
  , scheduleTick
  , scheduleTickAt
  , runTicker
  , runFrameTicker
  , levelToDelay
  ) where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

data Tick = Tick !Int | FrameTick !Int deriving (Eq, Show)
data Schedule = Schedule !Int !(Maybe Word64) deriving (Eq)
type Ticker = TVar Schedule

newTicker :: IO Ticker
newTicker = newTVarIO (Schedule 0 Nothing)

-- | Nothing suspends gravity. Changing the generation invalidates any event
-- already in the queue, including events from before pause, restart, or drop.
scheduleTick :: Ticker -> Maybe Int -> IO Int
scheduleTick ticker delay = do
  now <- getMonotonicTimeNSec
  scheduleTickAt ticker ((\micros -> now + fromIntegral (max 0 micros) * 1000) <$> delay)

-- | Absolute monotonic deadlines include time spent waiting for this worker.
scheduleTickAt :: Ticker -> Maybe Word64 -> IO Int
scheduleTickAt ticker deadline = atomically $ do
  Schedule generation _ <- readTVar ticker
  let next = generation + 1
  writeTVar ticker (Schedule next deadline)
  pure next

runTicker :: Ticker -> BChan Tick -> IO ()
runTicker = runTickerAs Tick

runFrameTicker :: Ticker -> BChan Tick -> IO ()
runFrameTicker = runTickerAs FrameTick

runTickerAs :: (Int -> Tick) -> Ticker -> BChan Tick -> IO ()
runTickerAs event ticker chan = forever $ do
  (current, generation, deadline) <- atomically $ do
    s@(Schedule generation delay) <- readTVar ticker
    case delay of
      Nothing -> retry
      Just deadline -> pure (s, generation, deadline)
  now <- getMonotonicTimeNSec
  let micros = if now >= deadline then 0 else fromIntegral $ (deadline - now + 999) `div` 1000
  timer <- registerDelay micros
  fired <- atomically $
    (do latest <- readTVar ticker
        check (latest /= current)
        pure False)
    `orElse`
    (do readTVar timer >>= check
        writeTVar ticker (Schedule generation Nothing)
        pure True)
  when fired $ writeBChan chan (event generation)

-- | Keep the existing gravity curve, with a 1ms floor to avoid zero-delay
-- spinning or exponent overflow for very large command-line levels.
levelToDelay :: Int -> Int
levelToDelay n
  | n <= 0 = 400000
  | n >= 19 = 1000
  | otherwise = max 1000 $ floor $ 400000 * (0.85 :: Double) ^ (2 * n)

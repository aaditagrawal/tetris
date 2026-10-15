{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module UI.Game
  ( playGame
  , playGameWithFPS
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (when)
import Prelude hiding (Left, Right)

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens hiding (preview, op, zoom)
import Control.Monad.Extra (orM)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform
import qualified Graphics.Vty.Config
import Data.List (find)
import GHC.Clock (getMonotonicTimeNSec)
import Linear.V2 (V2(..))

import Tetris
import UI.FrameClock
import UI.GravityClock
import UI.Grid
import UI.Image (compact, replaceRegion)
import UI.Ticker

data UI = UI
  { _game           :: Game
  , _initLevel      :: Int
  , _ticker         :: Ticker
  , _tickGeneration :: Int
  , _gravityClock   :: Maybe GravityClock
  , _frameTimer     :: Ticker
  , _frameGeneration :: Int
  , _framePending   :: Bool
  , _frameClock     :: FrameClock
  , _gridCache      :: Grid
  , _statsKey       :: (Int, Int, Int, Bool, Bool)
  , _nextKey        :: Tetrimino
  , _preview        :: Maybe String
  , _paused         :: Bool
  }

makeLenses ''UI

-- | Sidebar widgets stay unchanged during movement and rotation.
data Name = Stats | NextShape | Help | Frame | GridArea deriving (Eq, Ord, Show)

data VisualBlock
  = NormalBlock
  | HardDropBlock String

-- App definition and execution

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = syncTicker ResetGravity
  , appAttrMap      = const theMap
  }

playGame :: Int          -- ^ Starting level
          -> Maybe String  -- ^ Preview cell (Nothing == no preview)
          -> Bool         -- ^ Enable level progression
          -> IO Game
playGame = playGameWithFPS 120

playGameWithFPS :: Int -> Int -> Maybe String -> Bool -> IO Game
playGameWithFPS fps lvl mp prog = do
  chan <- newBChan 1
  ticker <- newTicker
  frameTimer <- newTicker
  initialGame <- initGame lvl prog  -- Pass the progression parameter
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  bracket (forkIO $ runTicker ticker chan) killThread $ \_ -> do
    bracket (forkIO $ runFrameTicker frameTimer chan) killThread $ \_ -> do
      initialVty <- buildVty
      now <- getMonotonicTimeNSec
      ui <- customMain initialVty buildVty (Just chan) app $ UI
        { _game           = initialGame
        , _initLevel      = lvl
        , _ticker         = ticker
        , _tickGeneration = 0
        , _gravityClock   = Nothing
        , _frameTimer     = frameTimer
        , _frameGeneration = 0
        , _framePending   = False
        , _frameClock     = newFrameClock fps now
        , _gridCache      = makeGameGrid initialGame
        , _statsKey       = gameStatsKey initialGame
        , _nextKey        = initialGame ^. nextShape
        , _preview        = mp
        , _paused         = False
        }
      return $ ui ^. game

-- | Apply input immediately, but combine changes within the same frame budget.
-- The frame timer is one-shot: an unchanged screen never starts a refresh loop.
requestFrame :: EventM Name UI ()
requestFrame = do
  now <- liftIO getMonotonicTimeNSec
  clock <- use frameClock
  let delay = frameDelayMicros now clock
  if delay == 0 then presentFrame else do
    pending <- use framePending
    when (not pending) $ do
      timer <- use frameTimer
      generation <- liftIO $ scheduleTick timer (Just delay)
      assign frameGeneration generation
      assign framePending True
    continueWithoutRedraw

presentFrame :: EventM Name UI ()
presentFrame = do
  now <- liftIO getMonotonicTimeNSec
  modifying frameClock (advanceFrameClock now)
  pending <- use framePending
  when pending $ do
    timer <- use frameTimer
    generation <- liftIO $ scheduleTick timer Nothing
    assign frameGeneration generation
    assign framePending False

data GravityUpdate = ResetGravity | AdvanceGravity

syncTicker :: GravityUpdate -> EventM Name UI ()
syncTicker mode = do
  inactive <- orM [use paused, use (game . to isGameOver)]
  lvl <- use $ game . level
  timer <- use ticker
  now <- liftIO getMonotonicTimeNSec
  previousClock <- use gravityClock
  let nextClock = if inactive then Nothing else Just $ case (mode, previousClock) of
        (AdvanceGravity, Just clock) -> advanceGravityClock (levelToDelay lvl) now clock
        _ -> newGravityClock (levelToDelay lvl) now
  generation <- liftIO $ scheduleTickAt timer (gravityDeadline <$> nextClock)
  assign tickGeneration generation
  assign gravityClock nextClock
  g <- use game
  oldStats <- use statsKey
  oldNext <- use nextKey
  when (oldStats /= gameStatsKey g) $ do
    invalidateCacheEntry Stats
    invalidateCacheEntry Frame
    assign statsKey (gameStatsKey g)
  when (oldNext /= g ^. nextShape) $ do
    invalidateCacheEntry NextShape
    invalidateCacheEntry Frame
    assign nextKey (g ^. nextShape)
  brd <- use $ game . board
  previous <- use $ gridCache . to gridBoard
  when (brd /= previous) $ assign gridCache $ makeGameGrid g

makeGameGrid :: Game -> Grid
makeGameGrid g = makeGrid (\t -> attrMapLookup (tToAttr t) theMap)
  (attrMapLookup emptyAttr theMap) (g ^. board)

gameStatsKey :: Game -> (Int, Int, Int, Bool, Bool)
gameStatsKey g = (g ^. score, g ^. linesCleared, g ^. level, g ^. progression, isGameOver g)

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = restart
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = exec (shift Right)
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift Left)
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = exec (shift Down)
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = exec (shift Right)
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = exec (shift Left)
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = exec (shift Down)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = exec rotate
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = exec rotate
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  unlessInactive $ do
    -- Lock and spawn in the same input event, without waiting for gravity.
    zoom game $ hardDrop >> timeStep
    syncTicker ResetGravity
    requestFrame
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = do
  over <- use (game . to isGameOver)
  if over then continueWithoutRedraw else do
    modifying paused not
    invalidateCacheEntry Frame
    syncTicker ResetGravity
    requestFrame
handleEvent (AppEvent (Tick generation)) = do
  current <- use tickGeneration
  if generation /= current then continueWithoutRedraw else unlessInactive $ do
    zoom game timeStep
    syncTicker AdvanceGravity
    requestFrame
handleEvent (AppEvent (FrameTick generation)) = do
  current <- use frameGeneration
  pending <- use framePending
  if generation /= current || not pending then continueWithoutRedraw else do
    assign framePending False
    requestFrame
handleEvent (VtyEvent (V.EvResize _ _)) = presentFrame
handleEvent _ = continueWithoutRedraw

-- | Ignore inactive input and blocked moves without rebuilding the screen.
exec :: Tetris () -> EventM Name UI ()
exec action = unlessInactive $ do
  before <- use $ game . block
  zoom game action
  after <- use $ game . block
  if before == after then continueWithoutRedraw else requestFrame

unlessInactive :: EventM Name UI () -> EventM Name UI ()
unlessInactive action = do
  inactive <- orM [use paused, use (game . to isGameOver)]
  if inactive then continueWithoutRedraw else action

-- | Restart game at the initially chosen level
restart :: EventM Name UI ()
restart = do
  lvl <- use initLevel
  prog <- use (game . progression)  -- Get current progression setting
  g <- liftIO $ initGame lvl prog   -- Use it when restarting
  assign game g
  assign paused False
  invalidateCacheEntry Frame
  syncTicker ResetGravity
  requestFrame

-- Drawing

drawUI :: UI -> [Widget Name]
drawUI ui
  | ui ^. paused = [frame]
  | otherwise = [frameWithBoard]
  where
    frame = cached Frame $ compact $ C.vCenter $ vLimit 22 $ hBox
      [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. game)
      , drawGridFrame ui
      , padRight Max $ padLeft (Pad 2) $ drawInfo (ui ^. game)
      ]
    frameWithBoard = Widget (hSize frame) (vSize frame) $ do
      background <- render frame
      case find ((== GridArea) . extentName) (background ^. extentsL) of
        Nothing -> pure background
        Just extent -> do
          let Location (x, y) = extentUpperLeft extent
              (w, h) = extentSize extent
          pure $ background & imageL %~ replaceRegion (x, y) (V.crop w h $ boardImage ui)

drawGridFrame :: UI -> Widget Name
drawGridFrame ui =
  hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Tetris")
    $ case ui ^. paused of
        True  -> C.center $ str "Paused"
        False -> reportExtent GridArea $ raw $ V.charFill V.defAttr ' ' (2 * boardWidth) boardHeight

boardImage :: UI -> V.Image
boardImage ui = gridImage (ui ^. gridCache) overlays
 where
  overlays = blockCells NormalBlock (ui ^. (game . block)) ++ case ui ^. preview of
    Nothing -> []
    Just s -> blockCells (HardDropBlock s) (evalTetris hardDroppedBlock (ui ^. game))
  blockCells v b = [(c, cellImage v (b ^. shape)) | c <- coords b]
  cellImage NormalBlock t = V.string (attrMapLookup (tToAttr t) theMap) " ."
  cellImage (HardDropBlock s) t = V.string (attrMapLookup (tToAttrH t) theMap) s

emptyNextShapeCellW :: Widget Name
emptyNextShapeCellW = withAttr emptyAttr ecw

drawBlockCell :: VisualBlock -> Tetrimino -> Widget Name
drawBlockCell NormalBlock       t = withAttr (tToAttr t) cw
drawBlockCell (HardDropBlock s) t = withAttr (tToAttrH t) (str s)

tToAttr :: Tetrimino -> AttrName
tToAttr I = iAttr
tToAttr O = oAttr
tToAttr T = tAttr
tToAttr S = sAttr
tToAttr Z = zAttr
tToAttr J = jAttr
tToAttr L = lAttr

tToAttrH :: Tetrimino -> AttrName
tToAttrH I = ihAttr
tToAttrH O = ohAttr
tToAttrH T = thAttr
tToAttrH S = shAttr
tToAttrH Z = zhAttr
tToAttrH J = jhAttr
tToAttrH L = lhAttr

cw :: Widget Name
cw = str " ."

ecw :: Widget Name
ecw = str "  "

drawStats :: Game -> Widget Name
drawStats g =
  cached Stats $ compact $ hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Stats")
    $ vBox
        [ drawStat "Score" $ g ^. score
        , padTop (Pad 1) $ drawStat "Lines" $ g ^. linesCleared
        , padTop (Pad 1) $ drawStat "Level" $ g ^. level
        , padTop (Pad 1) $ drawProgression (g ^. progression)
        , drawLeaderBoard g
        ]

drawProgression :: Bool -> Widget Name
drawProgression True =
    padLeftRight 1 $ str "Level Mode: " <+>
    withAttr progressionAttr (padLeft Max $ str "ON")
drawProgression False =
    padLeftRight 1 $ str "Level Mode: " <+>
    withAttr fixedAttr (padLeft Max $ str "Fixed")

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1 $ str s <+> padLeft Max (str $ show n)

drawLeaderBoard :: Game -> Widget Name
drawLeaderBoard _ = emptyWidget

drawInfo :: Game -> Widget Name
drawInfo g = hLimit 18 -- size of next piece box
  $ vBox
    [ drawNextShape (g ^. nextShape)
    , padTop (Pad 1) drawHelp
    , padTop (Pad 1) (drawGameOver g)
    ]

drawNextShape :: Tetrimino -> Widget Name
drawNextShape t =
  cached NextShape $ compact $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Next")
    $ padTopBottom 1
    $ padLeftRight 4
    $ vLimit 4
    $ vBox
    $ [0, -1] <&> \y ->
      hBox [ if V2 x y `elem` coords blk
             then drawBlockCell NormalBlock t
             else emptyNextShapeCellW
           | x <- [-2 .. 1]
           ]
  where blk = Block t (V2 0 0) (relCells t)

drawHelp :: Widget Name
drawHelp =
  cached Help $ compact $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Help")
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("Left"   , "h, ←")
      , ("Right"  , "l, →")
      , ("Down"   , "j, ↓")
      , ("Rotate" , "k, ↑")
      , ("Drop"   , "space")
      , ("Restart", "r")
      , ("Pause"  , "p")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if isGameOver g
  then padLeftRight 4 $ withAttr gameOverAttr $ str "GAME OVER"
  else emptyWidget

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (iAttr          , tToColor I `on` tToColor I)
  , (oAttr          , tToColor O `on` tToColor O)
  , (tAttr          , tToColor T `on` tToColor T)
  , (sAttr          , tToColor S `on` tToColor S)
  , (zAttr          , tToColor Z `on` tToColor Z)
  , (jAttr          , tToColor J `on` tToColor J)
  , (lAttr          , tToColor L `on` tToColor L)
  , (ihAttr         , fg $ tToColor I)
  , (ohAttr         , fg $ tToColor O)
  , (thAttr         , fg $ tToColor T)
  , (shAttr         , fg $ tToColor S)
  , (zhAttr         , fg $ tToColor Z)
  , (jhAttr         , fg $ tToColor J)
  , (lhAttr         , fg $ tToColor L)
  , (gameOverAttr   , fg V.red `V.withStyle` V.bold)
  , (progressionAttr, fg V.green `V.withStyle` V.bold)
  , (fixedAttr      , fg V.blue `V.withStyle` V.bold)
  ]

tToColor :: Tetrimino -> V.Color
tToColor I = V.cyan
tToColor O = V.yellow
tToColor T = V.magenta
tToColor S = V.green
tToColor Z = V.red
tToColor J = V.blue
tToColor L = V.white

iAttr, oAttr, tAttr, sAttr, zAttr, jAttr, lAttr :: AttrName
iAttr = attrName "I"
oAttr = attrName "O"
tAttr = attrName "T"
sAttr = attrName "S"
zAttr = attrName "Z"
jAttr = attrName "J"
lAttr = attrName "L"

ihAttr, ohAttr, thAttr, shAttr, zhAttr, jhAttr, lhAttr :: AttrName
ihAttr = attrName "Ih"
ohAttr = attrName "Oh"
thAttr = attrName "Th"
shAttr = attrName "Sh"
zhAttr = attrName "Zh"
jhAttr = attrName "Jh"
lhAttr = attrName "Lh"

emptyAttr :: AttrName
emptyAttr = attrName "empty"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

progressionAttr, fixedAttr :: AttrName
progressionAttr = attrName "progression"
fixedAttr = attrName "fixed"

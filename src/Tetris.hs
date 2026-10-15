{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Tetris
  (
  -- Game state modifiers
    initGame
  , timeStep
  , shift
  , rotate
  , hardDrop
  -- Game state handlers
  , evalTetris
  -- Game state queries
  , isGameOver
  , hardDroppedBlock
  , coords
  -- Types
  , Block(..)
  , Coord(..)
  , Direction(..)
  , Game(..)
  , Tetrimino(..)
  , Tetris
  -- Lenses
  , block, board, level, nextShape, score, shape, linesCleared, progression
  -- Constants
  , boardHeight, boardWidth, relCells
  ) where

import Prelude hiding (Left, Right)
import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter, when, (<=<))

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.State.Class (MonadState, gets, put)
import Control.Monad.Trans.State (evalStateT)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq(..), (<|), (><))
import qualified Data.Sequence as Seq
import Lens.Micro ((%~), (^.))
import Lens.Micro.Mtl ((%=), (.=), assign, modifying, use)
import Lens.Micro.TH (makeLenses)
import System.Random (getStdRandom, randomR)

import Tetris.Coord

-- Types and instances

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Tetris shape in location context
data Block = Block
  { _shape  :: Tetrimino -- ^ block type
  , _origin :: Coord -- ^ origin
  , _extra  :: [Coord] -- ^ extraneous cells
  } deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

-- | Board
--
-- If coordinate not present in map, yet in bounds, then it is empty,
-- otherwise its value is the type of tetrimino occupying it.
type Board = Map Coord Tetrimino

-- | Game state
data Game = Game
  { _level        :: Int
  , _block        :: Block
  , _nextShape    :: Tetrimino
  , _nextShapeBag :: Seq.Seq Tetrimino
  , _linesCleared :: Int
  , _score        :: Int
  , _board        :: Board
  , _progression  :: Bool
  } deriving (Eq)
makeLenses ''Game

evalTetris :: Tetris a -> Game -> a
evalTetris m = runIdentity . evalStateT m

type Tetris a = forall m. MonadState Game m => m a

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s
  translate = translateBy 1
  translateBy :: Int -> Direction -> s -> s

instance Translatable Coord where
  translateBy n Left (Coord x y)  = Coord (x-n) y
  translateBy n Right (Coord x y) = Coord (x+n) y
  translateBy n Down (Coord x y)  = Coord x (y-n)

instance Translatable Block where
  translateBy n d b =
    b & origin %~ translateBy n d
      & extra  %~ fmap (translateBy n d)

-- Low level functions on blocks and coordinates

initBlock :: Tetrimino -> Block
initBlock t = Block t startOrigin . fmap (addCoord startOrigin) . relCells $ t

relCells :: Tetrimino -> [Coord]
relCells I = map (uncurry Coord) [(-2, 0), (-1, 0), (1, 0)]
relCells O = map (uncurry Coord) [(-1, 0), (-1, -1), (0, -1)]
relCells S = map (uncurry Coord) [(-1, -1), (0, -1), (1, 0)]
relCells Z = map (uncurry Coord) [(-1, 0), (0, -1), (1, -1)]
relCells L = map (uncurry Coord) [(-1, -1), (-1, 0), (1, 0)]
relCells J = map (uncurry Coord) [(-1, 0), (1, 0), (1, -1)]
relCells T = map (uncurry Coord) [(-1, 0), (0, -1), (1, 0)]

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | Starting block origin
startOrigin :: Coord
startOrigin = Coord 6 22

-- | Enter the visible board immediately, respecting both hidden spawn rows.
-- Obstructions still stop entry and preserve the existing top-out behavior.
spawnBlock :: Board -> Tetrimino -> Block
spawnBlock brd = enter . enter . initBlock
  where
    enter blk = let candidate = translate Down blk
                in if isValidBlockPosition brd candidate then candidate else blk

-- | Rotate block counter clockwise about origin
-- *Note*: Strict unsafe rotation not respecting boundaries
-- Safety can only be assured within Game context
rotateRaw :: Block -> Block
rotateRaw b@(Block s o@(Coord xo yo) cs)
  | -- O doesn't need rotation
    s == O                             = b
  | -- I only has two orientations
    s == I && Coord xo (yo + 1) `elem` cs = rotateWith clockwise
  | otherwise                          = rotateWith counterclockwise
 where
  rotateWith dir = b & extra %~ fmap (addCoord o . dir . (`subtractCoord` o))

-- | Get coordinates of entire block
coords :: Block -> [Coord]
coords b = b ^. origin : b ^. extra

-- Higher level functions on game and board

-- | Facilitates cycling through at least 4 occurences of each shape
-- before next bag (random permutation of 4*each tetrimino) is created. If input is empty,
-- generates new bag, otherwise just unshifts the first value and returns pair.
bagFourTetriminoEach :: Seq.Seq Tetrimino -> IO (Tetrimino, Seq.Seq Tetrimino)
bagFourTetriminoEach (t :<| ts) = pure (t, ts)
bagFourTetriminoEach Empty =
  bagFourTetriminoEach <=< shuffle . Seq.fromList . take 28 $ cycle [I ..]

-- | Initialize a game with a given level
initGame :: Int -> Bool -> IO Game 
initGame lvl prog = do
  (s1, bag1) <- bagFourTetriminoEach mempty
  (s2, bag2) <- bagFourTetriminoEach bag1
  pure $ Game
    { _level        = lvl
    , _block        = spawnBlock mempty s1
    , _nextShape    = s2
    , _nextShapeBag = bag2
    , _score        = 0
    , _linesCleared = 0
    , _board        = mempty
    , _progression  = prog  
    }

-- | Increment level
nextLevel :: (MonadIO m, MonadState Game m) => m ()
nextLevel = do
  level %= (+ 1)

isGameOver :: Game -> Bool
isGameOver g = blockStopped g && g ^. (block . origin) == startOrigin

-- | The main game execution, this is executed at each discrete time step
timeStep :: (MonadIO m, MonadState Game m) => m ()
timeStep = do
  gets blockStopped >>= \case
    False -> gravitate
    True -> do
      freezeBlock
      clearFullRows >>= updateScore
      prog <- use progression
      when prog $ do
        levelFinished >>= \case
          True -> nextLevel
          False -> pure ()
      nextBlock

-- | Gravitate current block, i.e. shift down
gravitate :: MonadState Game m => m ()
gravitate = shift Down

-- | If necessary: clear full rows and return the count
clearFullRows :: MonadState Game m => m Int
clearFullRows = do
  brd <- use board
  let rowCounts = M.foldlWithKey' (\counts (Coord _ y) _ -> IM.insertWith (+) y (1 :: Int) counts) IM.empty brd
      fullRows = IM.keys $ IM.filterWithKey (\y count -> y >= 1 && y <= boardHeight && count == boardWidth) rowCounts
  let clearedLines = length fullRows
  when (clearedLines > 0) $ do
    -- Only rebuild the board when a line was actually cleared.
    board .= M.mapKeysMonotonic (\(Coord x y) -> Coord x (y - length (filter (< y) fullRows)))
      (M.filterWithKey (\(Coord _ y) _ -> y `notElem` fullRows) brd)
    linesCleared %= (+ clearedLines)
  pure clearedLines

-- | This updates game points with respect to the provided number of cleared
-- lines.
--
-- See https://tetris.fandom.com/wiki/Scoring
updateScore :: (MonadState Game m, MonadIO m) => Int -> m ()
updateScore 0 = pure ()
updateScore lines = do
  lvl <- use level
  let newPoints = (lvl + 1) * points lines
  score %= (+ newPoints)
  where
    -- Translate row line clears to points
    points 0 = 0
    points 1 = 40
    points 2 = 100
    points 3 = 300
    points _ = 1200

-- | Using the fixed-goal system described here: https://tetris.wiki/Marathon
levelFinished :: (MonadState Game m, MonadIO m) => m Bool
levelFinished = do
  prog <- use progression
  if not prog
    then pure False
    else do
      lvl <- use level
      lc <- use linesCleared
      pure $ lvl < 15 && lc >= 10 * (lvl + 1)

-- | Handle counterclockwise block rotation (if possible)
-- Allows wallkicks: http://tetris.wikia.com/wiki/TGM_rotation
rotate :: MonadState Game m => m ()
rotate = do
  blk <- use block
  brd <- use board
  let mblk = foldr (<|>) Nothing
        $   mfilter (isValidBlockPosition brd)
        .   pure
        .   ($ blk)
        <$> [ rotateRaw
            , rotateRaw . translate Left
            , rotateRaw . translate Right
            ]
  forM_ mblk $ assign block

blockStopped :: Game -> Bool
blockStopped g = isStopped (g ^. board) (g ^. block)

-- | Check if a block on a board is stopped from further gravitation
isStopped :: Board -> Block -> Bool
isStopped brd = any stopped . coords
 where
  stopped = (||) <$> atBottom <*> (`M.member` brd) . translate Down
  atBottom (Coord _ y) = y == 1

hardDrop :: MonadState Game m => m ()
hardDrop = hardDroppedBlock >>= assign block

hardDroppedBlock :: MonadState Game m => m Block
hardDroppedBlock = do
  brd <- use board
  blk <- use block
  -- Coord ordering is column, then height. The predecessor is the nearest
  -- occupied cell below us if it belongs to the same column.
  let clearance c@(Coord x y) = case M.lookupLT c brd of
        Just (Coord obstacleX obstacleY, _) | obstacleX == x -> y - obstacleY - 1
        _ -> y - 1
      dist = minimum $ clearance <$> coords blk
  pure $ translateBy dist Down blk

-- | Freeze current block
freezeBlock :: MonadState Game m => m ()
freezeBlock = do
  blk <- use block
  modifying board $ M.union $ M.fromList [ (c, _shape blk) | c <- coords blk ]

-- | Replace block with next block
nextBlock :: (MonadIO m, MonadState Game m) => m ()
nextBlock = do
  bag <- use nextShapeBag
  (t, ts) <- liftIO $ bagFourTetriminoEach bag
  brd <- use board
  use nextShape >>= \s -> block .= spawnBlock brd s
  nextShape .= t
  nextShapeBag .= ts

-- | Try to shift current block; if shifting not possible, leave block where it is
shift :: MonadState Game m => Direction -> m ()
shift dir = do
  brd <- use board
  blk <- use block
  let candidate = translate dir blk
  when (isValidBlockPosition brd candidate) $
    block .= candidate

-- | Check if coordinate is already occupied or free in board
isFree :: Board -> Coord -> Bool
isFree = flip M.notMember

-- | Check if coordinate is in or out of bounds
isInBounds :: Coord -> Bool
isInBounds (Coord x y) = 1 <= x && x <= boardWidth && 1 <= y

-- | Checks if block's potential new location is valid
isValidBlockPosition :: Board -> Block -> Bool
isValidBlockPosition brd = all validCoord . coords
  where validCoord = (&&) <$> isFree brd <*> isInBounds

-- General utilities

-- | Shuffle a sequence (random permutation)
shuffle :: Seq.Seq a -> IO (Seq.Seq a)
shuffle xs
  | null xs = mempty
  | otherwise = do
    randomPosition <- getStdRandom (randomR (0, length xs - 1))
    case Seq.splitAt randomPosition xs of
      (left, y :<| ys) ->  fmap (y <|) (shuffle $ left >< ys)
      _ -> error "impossible"

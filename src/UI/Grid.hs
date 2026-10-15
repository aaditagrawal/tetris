-- | Cache the settled board and its rendered rows between piece movements.
module UI.Grid
  ( Grid
  , makeGrid
  , gridBoard
  , gridImage
  ) where

import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import qualified Graphics.Vty as V

import Tetris (Coord(..), Tetrimino, boardHeight, boardWidth)

data Grid = Grid
  { gridBoard :: Map Coord Tetrimino
  , settledRows :: IM.IntMap (IM.IntMap V.Image)
  , renderedRows :: IM.IntMap V.Image
  , emptySpans :: IM.IntMap V.Image
  }

makeGrid :: (Tetrimino -> V.Attr) -> V.Attr -> Map Coord Tetrimino -> Grid
makeGrid shapeAttr emptyAttr brd = grid
  where
    grid = Grid brd rows images blanks
    rows = M.foldlWithKey' insertCell IM.empty brd
    insertCell result (Coord x y) t
      | visible x y = IM.insertWith IM.union y (IM.singleton x $ V.string (shapeAttr t) " .") result
      | otherwise = result
    blanks = IM.fromDistinctAscList $ (0, V.emptyImage) :
      [(n, V.string emptyAttr $ concat $ replicate n " .") | n <- [1 .. boardWidth]]
    images = IM.fromDistinctAscList
      [(y, renderRow grid $ IM.findWithDefault IM.empty y rows) | y <- [1 .. boardHeight]]

-- | Overlay order is first-wins; settled cells retain precedence over overlays.
-- Untouched rows reuse a single image instead of ten cell images.
gridImage :: Grid -> [(Coord, V.Image)] -> V.Image
gridImage grid overlays = V.vertCat
  [ case IM.lookup y patches of
      Nothing -> renderedRows grid IM.! y
      Just row -> renderRow grid $ IM.union (IM.findWithDefault IM.empty y $ settledRows grid) row
  | y <- [boardHeight, boardHeight - 1 .. 1]
  ]
  where
    patches = foldr insertCell IM.empty overlays
    insertCell (Coord x y, image) result
      | visible x y = IM.insertWith IM.union y (IM.singleton x image) result
      | otherwise = result

renderRow :: Grid -> IM.IntMap V.Image -> V.Image
renderRow grid = V.horizCat . segments 1 . IM.toAscList
  where
    blank n = emptySpans grid IM.! n
    segments x [] = [blank $ boardWidth - x + 1]
    segments x ((next, image):rest) =
      blank (next - x) : image : segments (next + 1) rest

visible :: Int -> Int -> Bool
visible x y = x >= 1 && x <= boardWidth && y >= 1 && y <= boardHeight

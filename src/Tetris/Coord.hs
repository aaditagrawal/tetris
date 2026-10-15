module Tetris.Coord
  ( Coord(..)
  , addCoord
  , subtractCoord
  , clockwise
  , counterclockwise
  ) where

-- | Board coordinates, ordered by column first, then height. Hard-drop
-- predecessor searches and monotonic row shifts rely on this ordering.
data Coord = Coord !Int !Int
  deriving (Eq, Ord, Show)

addCoord :: Coord -> Coord -> Coord
addCoord (Coord x y) (Coord dx dy) = Coord (x + dx) (y + dy)

subtractCoord :: Coord -> Coord -> Coord
subtractCoord (Coord x y) (Coord dx dy) = Coord (x - dx) (y - dy)

clockwise :: Coord -> Coord
clockwise (Coord x y) = Coord y (-x)

counterclockwise :: Coord -> Coord
counterclockwise (Coord x y) = Coord (-y) x

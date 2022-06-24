module Utility where

import CodeWorld.Reflex (Point)
import Constants
import Data.Maybe (listToMaybe)
import Datatype

-- | Add coordinates to board cells.
enumerateBoard :: [[a]] -> [[(Coords, a)]]
enumerateBoard = zipWith enumerateRow [0 ..]
  where
    enumerateRow index row = zipWith (packCell index) [0 ..] row
    packCell y x cell = ((x, y), cell)

fromCoords :: Coords -> (Double, Double)
fromCoords (x, y) = (fromIntegral x, fromIntegral y)

-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index f list = first ++ secondPart
  where
    (first, second) = splitAt index list
    secondPart =
      case second of
        [] -> []
        (x : xs) -> f x : xs

-- | Convert mouse position into board coordinates.
pointToCoords :: Point -> (Int, Int)
pointToCoords coords = (round (x / cellSize), round (y / cellSize))
  where
    (x, y) = shiftPoint coords shift

-- | Get i-th element of the given list or Nothing if element index is out of bounds.
safeGetAt :: Int -> [a] -> Maybe a
safeGetAt index list = listToMaybe (take 1 (drop index list))

-- | Shift vector to put board in the center of the screen.
shift :: Point
shift = (sizeToShift boardWidth, sizeToShift boardHeight)
  where
    sizeToShift s = fromIntegral s / 2 * cellSize

-- | Apply shift on given point.
shiftPoint :: Point -> Point -> Point
shiftPoint (x, y) (dx, dy) = (x + dx, y + dy)

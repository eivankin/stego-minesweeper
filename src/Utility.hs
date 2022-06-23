module Utility where

import Datatype
import CodeWorld.Reflex (Point)
import Constants (cellSize)
import Data.Maybe (listToMaybe, fromMaybe)

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
pointToCoords (x, y) = (round (x / cellSize), round (y / cellSize))

safeGetAt :: Int -> [a] -> Maybe a
safeGetAt index list = listToMaybe (take 1 (drop index list))

width :: Board -> Int 
width board = length firstRow
  where
    firstRow = fromMaybe [] (listToMaybe (take 1 board))

height :: Board -> Int
height = length

getShift :: Board -> Point
getShift board = (-fromIntegral (width board) / 2, -fromIntegral (height board) / 2)

shiftPoint :: Point -> Point -> Point
shiftPoint (x, y) (dx, dy) = (x - dx, y - dy)
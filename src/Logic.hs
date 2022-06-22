module Logic (openCellWithNeighbors, minesToBoard, disarmBomb) where

import Data.Maybe
import Datatype
import Utility

openCell :: Coords -> Board -> Board
openCell (x, y) = updateAt y (updateAt x processCell)
  where
    processCell (Cell content Closed) = Cell content Opened
    processCell cell = cell

openCellWithNeighbors :: Coords -> Game -> Game
openCellWithNeighbors (x, y) game@(state, board)
  | state /= InProcess = game
  | x >= 0 && x < length firstRow && y >= 0 && y < length board =
    case safeGetAt x (fromMaybe [] (safeGetAt y board)) of
      Just (Cell (Neighbors Nothing) Closed) -> (InProcess, neighbors board)
      Just (Cell Bomb Closed) -> (Lose, openCell (x, y) board)
      _ -> (InProcess, openCell (x, y) board)
  | otherwise = game
  where
    firstRow = fromMaybe [] (listToMaybe (take 1 board))
    neighbors =
      foldr
        (.)
        (openCell (x, y))
        ( concatMap
            ( \posY ->
                map
                  (\posX -> openNeighbors (posX, posY))
                  [x - 1 .. x + 1]
            )
            [y - 1 .. y + 1]
        )
    openNeighbors coords gameBoard = snd $ openCellWithNeighbors coords (InProcess, gameBoard)

minesToBoard :: [[Bool]] -> Board
minesToBoard board = map (map boolToCell) (enumerateBoard board)
  where
    boolToCell (_, True) = Cell Bomb Closed
    boolToCell (coords, False) = Cell (Neighbors (countNeighbors coords)) Closed

    countNeighbors coords = intToNeighborsCount (length (filter id (getNeighbors coords)))
    getNeighbors coords = getNeighborsAt coords False board

disarmBomb :: Coords -> Board -> Board
disarmBomb (x, y) board = updateAt y (updateAt x processCell) board
  where
    processCell (Cell Bomb Closed) = Cell (Neighbors (countNeighbors (x, y))) Closed
    processCell cell = cell
    countNeighbors coords = intToNeighborsCount (length (filter isBomb (getNeighbors coords)))
    getNeighbors coords = getNeighborsAt coords (Cell (Neighbors Nothing) Closed) board

    isBomb (Cell Bomb _) = True
    isBomb _ = False

getNeighborsAt :: Coords -> a -> [[a]] -> [a]
getNeighborsAt (x, y) defaultValue board =
  concatMap
    ( \posY ->
        map
          (\posX -> fromMaybe defaultValue (safeGetAt posX (fromMaybe [] (safeGetAt posY board))))
          [x - 1 .. x + 1]
    )
    [y - 1 .. y + 1]

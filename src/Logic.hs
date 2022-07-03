module Logic (openCellWithNeighbors, minesToBoard, markCell, checkWin, anticlockwiseCoords, toBoard) where

import CodeWorld.Reflex (Vector)
import Constants (boardHeight, boardWidth)
import Data.List (sortOn)
import Data.List.Split
import Data.Maybe
import Datatype
import Utility
import Prelude hiding (Left, Right)

-- | Open closed cell at given coordinates.
openCell :: Coords -> Board -> Board
openCell = updateCell open
  where
    open (Cell content Closed) = Cell content Opened
    open cell = cell

-- | Get cell at given coordinates from given board.
getCell :: Coords -> Board -> Maybe Cell
getCell (x, y) board = safeGetAt x (fromMaybe [] (safeGetAt y board))

-- | Update board cell at given coordinates with given function.
updateCell :: (Cell -> Cell) -> Coords -> Board -> Board
updateCell updater (x, y) = updateAt y (updateAt x updater)

-- | Check if player won and update game state accordingly.
checkWin :: Game -> Game
checkWin game@(mode, state) =
  case state of
    (InProcess board) -> if and (concatMap (map isBombOrOpen) board) then (mode, Win board) else game
    _ -> game
  where
    isBombOrOpen (Cell Bomb _) = True
    isBombOrOpen (Cell _ Opened) = True
    isBombOrOpen _ = False

-- | Wraps 'openCell' with game state updating and neighbors opening (if needed).
openCellWithNeighbors :: Coords -> Game -> Game
openCellWithNeighbors currentCoords@(x, y) game@(mode, state) =
  case state of
    (InProcess board) ->
      if isCoordsOnBoard currentCoords
        then case getCell (x, y) board of
          Just (Cell (Neighbors Nothing) Closed) -> (mode, InProcess (neighbors board))
          Just (Cell Bomb Closed) -> (mode, Lose (openCell (x, y) board) (x, y))
          _ -> (mode, InProcess (openCell (x, y) board))
        else game
    _ -> game
  where
    neighbors = foldNeighbors (openCell (x, y)) openNeighbors (x, y)
    openNeighbors coords gameBoard =
      extractBoard $
        snd $
          openCellWithNeighbors coords (mode, InProcess gameBoard)

-- | Convert 2D boolean array to the valid board.
minesToBoard :: RawBoard -> Board
minesToBoard board = map (map boolToCell) (enumerateBoard board)
  where
    boolToCell (_, True) = Cell Bomb Closed
    boolToCell (coords, False) = Cell (Neighbors (countNeighbors coords)) Closed

    countNeighbors coords = intToNeighborsCount (length (filter id (getNeighbors coords)))
    getNeighbors coords = getNeighborsAt coords False board

-- | Get 1D list of neighbor elements in 2D array.
getNeighborsAt :: Coords -> a -> [[a]] -> [a]
getNeighborsAt (x, y) defaultValue board =
  concatMap
    ( \posY ->
        map
          (\posX -> fromMaybe defaultValue (safeGetAt posX (fromMaybe [] (safeGetAt posY board))))
          [max (x - 1) 0 .. x + 1]
    )
    [max (y - 1) 0 .. y + 1]

-- | Mark cell at given coordinates.
markCell :: Coords -> Board -> Board
markCell (x, y) = updateAt y (updateAt x processCell)
  where
    processCell (Cell content Closed) = Cell content Flagged
    processCell (Cell content Flagged) = Cell content Marked
    processCell (Cell content Marked) = Cell content Closed
    processCell cell = cell

-- | Apply given functions on neighbor cells.
foldNeighbors :: (Num a1, Num a2, Enum a1, Enum a2) => (a3 -> b) -> ((a1, a2) -> b -> b) -> (a1, a2) -> a3 -> b
foldNeighbors startFunc stepFunc (x, y) =
  foldr
    (.)
    startFunc
    ( concatMap
        ( \posY ->
            map
              (\posX -> stepFunc (posX, posY))
              [x - 1 .. x + 1]
        )
        [y - 1 .. y + 1]
    )

-- | Get next direction (make  turn) and number of steps to move anticlockwise.
nextDir :: Int -> Dir -> (Dir, Int)
nextDir initialStepCount currentDir =
  case currentDir of
    Left -> (Down, initialStepCount)
    Down -> (Right, initialStepCount + 1)
    Right -> (Up, initialStepCount)
    Up -> (Left, initialStepCount + 1)

-- | Generate spiral anticlockwise sequence of coordinates starting lying on board from given point.
anticlockwiseCoords :: Vector -> [Coords]
anticlockwiseCoords (i, j) =
  filter
    isCoordsOnBoard
    ( map
        (vectorToCoords . fst)
        (iterate makeStep (start, (startSize, Left, startSize)))
    )
  where
    makeStep (currentPos, (initialStepCount, dir, remainingSteps)) = (newPos, newInfo)
      where
        newPos = move dir currentPos
        newInfo
          | remainingSteps < 2 = let (newDir, newSteps) = nextDir initialStepCount dir in (newSteps, newDir, newSteps)
          | otherwise = (initialStepCount, dir, remainingSteps - 1)
    vectorToCoords (x, y) = (round x, round y)
    startSize = 1
    start = (j, i)

-- | Convert 1D raw board with enumerated cells to an actual 2D board.
toBoard :: [(Coords, a)] -> [[a]]
toBoard = chunksOf boardWidth . map snd . sortOn fst . take (boardWidth * boardHeight)

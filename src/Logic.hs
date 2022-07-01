module Logic (openCellWithNeighbors, minesToBoard, disarmBomb, markCell, checkWin) where

import Constants
import Data.Maybe
import Datatype
import Utility

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
checkWin (mode, state, board)
  | not (isTerminalState state) && and (concatMap (map isBombOrOpen) board) = (mode, Win, board)
  | otherwise = (mode, state, board)
  where
    isBombOrOpen (Cell Bomb _) = True
    isBombOrOpen (Cell _ Opened) = True
    isBombOrOpen _ = False

-- | Wraps 'openCell' with game state updating and neighbors opening (if needed).
openCellWithNeighbors :: Coords -> Game -> Game
openCellWithNeighbors (x, y) game@(mode, state, board)
  | state /= InProcess = game
  | x >= 0 && x < boardWidth && y >= 0 && y < boardHeight =
    case getCell (x, y) board of
      Just (Cell (Neighbors Nothing) Closed) -> (mode, InProcess, neighbors board)
      Just (Cell Bomb Closed) -> (mode, Lose (x, y), openCell (x, y) board)
      _ -> (mode, InProcess, openCell (x, y) board)
  | otherwise = game
  where
    neighbors = foldNeighbors (openCell (x, y)) openNeighbors (x, y)
    openNeighbors coords gameBoard = third $ openCellWithNeighbors coords (mode, InProcess, gameBoard)
    third (_, _, v) = v

-- | Convert 2D boolean array to the valid board.
minesToBoard :: RawBoard -> Board
minesToBoard board = map (map boolToCell) (enumerateBoard board)
  where
    boolToCell (_, True) = Cell Bomb Closed
    boolToCell (coords, False) = Cell (Neighbors (countNeighbors coords)) Closed

    countNeighbors coords = intToNeighborsCount (length (filter id (getNeighbors coords)))
    getNeighbors coords = getNeighborsAt coords False board

-- | Disarm bomb and given coordinates and update neighbors.
disarmBomb :: Coords -> Board -> Board
disarmBomb (x, y) = foldNeighbors (updateCell processCell (x, y)) updateCount (x, y)
  where
    processCell (Cell Bomb Closed) = Cell (Neighbors Nothing) Closed
    processCell cell = cell

    updateCount coords currentBoard =
      -- Update neighbor bomb count for a cell at given coordinates.
      case getCell coords currentBoard of
        Just (Cell (Neighbors _) state) ->
          updateCell
            (const (Cell (Neighbors (countNeighbors coords currentBoard)) state))
            coords
            currentBoard
        _ -> currentBoard

    countNeighbors coords currentBoard =
      -- Count neighbor bombs.
      intToNeighborsCount (length (neighborBombs coords currentBoard))
    neighborBombs coords currentBoard = filter isBomb (getNeighbors coords currentBoard) -- Get list of neighbor bombs.
    getNeighbors coords currentBoard =
      -- Get list of all neighbor cells.
      getNeighborsAt coords (Cell (Neighbors Nothing) Closed) currentBoard

    isBomb (Cell Bomb _) = True
    isBomb _ = False

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

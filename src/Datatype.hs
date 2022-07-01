module Datatype where

import Constants ( boardWidth, boardHeight)


-- | Board cell coordinates.
type Coords = (Int, Int)

-- | Cell has 3 states:
-- 1. Opened state - player the cell content.
-- 2. Closed state - player can open the cell.
-- 3. Flagged or marked state - player cannot open the cell, but can unmark it or change mark.
data CellState = Flagged | Marked | Closed | Opened deriving (Show)

-- | Safe number of neighbor bombs.
type NeighborsCount = Maybe Int

-- | Convert an integer to the count of neighbor bombs.
intToNeighborsCount :: Int -> NeighborsCount
intToNeighborsCount number
  | 0 < number && number < 9 = Just number
  | otherwise = Nothing

-- | Cell content: bomb or the number of neighbor bombs (if any).
data CellContent = Bomb | Neighbors NeighborsCount deriving (Show)

-- | Cell can be opened or closed, has some content and may have a mark.
data Cell = Cell CellContent CellState deriving (Show)

-- | Game has 3 types of states:
-- 1. Initial state ('Start') - if player clicks on bomb, it will be disarmed, e.g. player cannot lose.
-- 2. Process state ('InProcess') - player can lose.
-- 3. End state ('Win' or 'Lose') - player cannot make turns.
-- 'Lose' state also stores coordinates of the last player's move.
data GameState = Start [Bool] | InProcess Board | Win Board | Lose Board Coords deriving (Show)

-- | Check if state is terminate, e.g. cannot be changed.
-- See 'GameState' docs for more information about states.
isTerminalState :: GameState -> Bool
isTerminalState (Win _) = True
isTerminalState (Lose _ _) = True
isTerminalState _ = False

-- | Game has 2 types of clicks:
-- 1. Cell opening - player can open unmarked cells.
-- 2. Cell marking - player can change type of cell mark.
data ClickMode = OpenCell | MarkCell deriving (Show)

-- | Board is a 2D array of cells.
type Board = [[Cell]]

emptyBoard :: Board
emptyBoard = replicate boardHeight (replicate boardWidth (Cell (Neighbors Nothing) Closed))

extractBoard :: GameState -> Board
extractBoard (InProcess board) = board
extractBoard (Win board) = board
extractBoard (Lose board _) = board
extractBoard _ = emptyBoard

-- | Raw board is a 2D array of boolean that indicates whether there is a bomb in the cell.
type RawBoard = [[Bool]]

-- | Usual game.
type Game = (ClickMode, GameState)

-- | Sequence of games.
type MultiBoardGame = (ClickMode, GameState, [[Bool]])

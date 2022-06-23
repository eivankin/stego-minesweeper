module Datatype where

type Coords = (Int, Int)

-- | Cell has 3 states:
-- 1. Opened state - player the cell content.
-- 2. Closed state - player can open the cell.
-- 3. Flagged or marked state - player cannot open the cell, but can unmark it or change mark.
data CellState = Flagged | Marked | Closed | Opened

-- | Safe number of neighbor bombs.
type NeighborsCount = Maybe Int

-- | Convert an integer to the count of neighbor bombs.
intToNeighborsCount :: Int -> NeighborsCount
intToNeighborsCount number
  | 0 < number && number < 9 = Just number
  | otherwise = Nothing

-- | Cell content: bomb or the number of neighbor bombs (if any).
data CellContent = Bomb | Neighbors NeighborsCount

-- | Cell can be opened or closed, has some content and may have a mark.
data Cell = Cell CellContent CellState

-- | Game has 3 states:
-- 1. Initial state ('Start') - timer is stopped, board is not initialized, player can make first turn.
-- 2. Process state ('InProcess') - timer is running, player can make turns.
-- 3. End state ('Win' or 'Lose') - timer is stopped, player cannot make turns.
-- 'Lose' state also stores coordinates of the last player's move.
data GameState = Start | InProcess | Win | Lose Coords deriving (Eq)

data ClickMode = OpenCell | MarkCell

-- | Board is a 2D array of cells.
type Board = [[Cell]]

-- | Alias for tuple of game state and board.
type Game = (ClickMode, GameState, Board)

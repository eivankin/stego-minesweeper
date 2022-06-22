module Minesweeper (run) where

import CodeWorld
import Datatype
import Logic
import Rendering
import System.Random
import Utility

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 10

createBoard :: (RandomGen g) => Double -> g -> Board
createBoard density stdGen = minesToBoard (map makeRow (take boardHeight (randoms stdGen)))
  where
    makeRow seed = map (< density) (take boardWidth (randoms (mkStdGen seed)))

-- | Handle mouse clicks to put marks.
handleGame :: Event -> Game -> Game
handleGame (PointerPress mouse) (Start, board) = 
  openCellWithNeighbors (pointToCoords mouse) (InProcess, disarmBomb (pointToCoords mouse) board)
handleGame (PointerPress mouse) game@(InProcess, _) = openCellWithNeighbors (pointToCoords mouse) game
handleGame _ game = game

run :: IO ()
run = do
  stdGen <- getStdGen
  let board = createBoard 0.2 stdGen
  activityOf (Start, board) handleGame drawBoard

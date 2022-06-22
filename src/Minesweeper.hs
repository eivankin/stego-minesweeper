module Minesweeper (run) where

import CodeWorld
import Datatype
import Rendering
import Logic
import Utility

sampleBoard :: Board
sampleBoard =
  [ map (\ n -> Cell (Neighbors (Just n)) Opened) [1..8],
  map (Cell Bomb) [Opened, Closed, Marked, Flagged]
  ]

-- | Handle mouse clicks to put marks.
handleGame :: Event -> Board -> Board
handleGame (PointerPress mouse) = openCell (pointToCoords mouse)
handleGame _ = id

run :: IO ()
run = activityOf sampleBoard handleGame drawBoard

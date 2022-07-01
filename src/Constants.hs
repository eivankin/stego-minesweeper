{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Constants where

-- | Cell height and width for rendering purposes.
cellSize :: Double
cellSize = 1

-- | Distance between cell edge and content for rendering purposes.
cellPadding :: Double
cellPadding = 0.1 * cellSize

-- | Board width for board generation.
boardWidth :: Int
boardWidth = 9

-- | Board width for board generation.
boardHeight :: Int
boardHeight = 9

numberOfMines :: Int
numberOfMines = 9
  
indicatorLen :: Int
indicatorLen = ceiling (logBase 2 (fromIntegral numberOfMines))
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

-- | Number of mines on board, used to encode message as an sequence of valid boards.
numberOfMines :: Int
numberOfMines = 9

-- | Number of bits to store the number of missing mines on the last board
indicatorLen :: Int
indicatorLen = ceiling (logBase 2 (fromIntegral numberOfMines))

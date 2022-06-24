{-# LANGUAGE OverloadedStrings #-}

module Minesweeper (run) where

import CodeWorld
import Constants
import Datatype
import Logic
import Rendering
import System.Random
import Utility

-- | Create random board.
createBoard :: (RandomGen g) => Double -> g -> Board
createBoard density stdGen = minesToBoard (map makeRow (take boardHeight (randoms stdGen)))
  where
    makeRow seed = map (< density) (take boardWidth (randoms (mkStdGen seed)))

-- | The type of an 'activityOf' function.
type ActivityOf world =
  world ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()

-- | Make 'activityOf' resettable on Esc.
withReset :: ActivityOf world -> ActivityOf world
withReset activity initState handleEvent = activity initState handleEvent'
  where
    handleEvent' event state =
      case event of
        KeyPress "Esc" -> initState
        other -> handleEvent other state

-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = StartScreen -- ˆ Start screen.
  | GameOn world -- ˆ Game is on with 'world' state.

-- | Add start screen to 'activityOf'.
withStartingScreen :: ActivityOf (WithStartScreen world) -> ActivityOf world
withStartingScreen activity initState handleEvent drawState = activity StartScreen handleEvent' drawState'
  where
    handleEvent' event StartScreen =
      case event of
        KeyPress " " -> GameOn initState
        _ -> StartScreen
    handleEvent' event (GameOn state) = GameOn (handleEvent event state)

    drawState' StartScreen = startingScreen
    drawState' (GameOn state) = drawState state

-- | Handle mouse clicks to put marks.
handleGame :: Event -> Game -> Game
handleGame (PointerPress mouse) (OpenCell, Start, board) =
  checkWin (openCellWithNeighbors clickPos (OpenCell, InProcess, disarmBomb clickPos board))
  where
    clickPos = pointToCoords mouse
handleGame (PointerPress mouse) game@(OpenCell, InProcess, _) =
  checkWin (openCellWithNeighbors (pointToCoords mouse) game)
handleGame (KeyPress "Ctrl") (_, state, board) = (MarkCell, state, board)
handleGame (KeyRelease "Ctrl") (_, state, board) = (OpenCell, state, board)
handleGame (PointerPress mouse) game@(MarkCell, state, board) =
  case state of
    Win -> game
    Lose _ -> game
    _ -> (MarkCell, state, markCell (pointToCoords mouse) board)
handleGame _ game = game

-- | Default entry point.
run :: IO ()
run = do
  stdGen <- getStdGen
  (withReset . withStartingScreen) activityOf (OpenCell, Start, createBoard 0.1 stdGen) handleGame drawGame

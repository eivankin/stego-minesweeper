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
createBoard :: Double -> Int -> Board
createBoard density seed = minesToBoard (map makeRow (take boardHeight (randoms (mkStdGen seed))))
  where
    makeRow rowSeed = map (< density) (take boardWidth (randoms (mkStdGen rowSeed)))

-- | The type of an 'activityOf' function.
type ActivityOf world =
  world ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()

-- | Make 'activityOf' resettable on Esc and if terminal state is reached.
withMultipleGames :: ActivityOf MultiBoardGame -> ActivityOf MultiBoardGame
withMultipleGames activity initialGame@(initialMode, initialGameState, _) handleEvent = activity initialGame handleEvent'
  where
    handleEvent' event game@(_, state, boards) =
      case event of
        KeyPress "Esc" ->
          if isTerminalState state
            then (initialMode, initialGameState, drop 1 boards)
            else game
        other -> handleEvent other game

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

-- | Create a new game with StdGen.
initialState :: StdGen -> MultiBoardGame
initialState stdGen = (OpenCell, Start, map (createBoard 0.1) (randoms stdGen))

-- | Default entry point.
run :: IO ()
run = do
  stdGen <- getStdGen
  (withMultipleGames . withStartingScreen) activityOf (initialState stdGen) multiBoardHandle multiBoardDraw
  where
    multiBoardHandle event (mode, state, board : remainingBoards) = (newMode, newState, newBoard : remainingBoards)
      where
        (newMode, newState, newBoard) = handleGame event (mode, state, board)
    multiBoardHandle _ state@(_, _, []) = state
    multiBoardDraw (mode, state, board : _) = drawGame (mode, state, board)
    multiBoardDraw (_, _, []) = endScreen

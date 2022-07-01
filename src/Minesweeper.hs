{-# LANGUAGE OverloadedStrings #-}

module Minesweeper (run) where

import CodeWorld
import Datatype
import Logic
import MessageEncoding
import Rendering
import System.Random
import Utility

-- | The type of an 'activityOf' function.
type ActivityOf world =
  world ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()

-- | Make 'activityOf' resettable on Esc and if terminal state is reached.
withMultipleGames :: ActivityOf MultiBoardGame -> ActivityOf MultiBoardGame
withMultipleGames activity initialGame@(initialMode, _, _) handleEvent = activity initialGame handleEvent'
  where
    handleEvent' event game@(_, state, boards) =
      case event of
        KeyPress "Esc" ->
          if isTerminalState state
            then (initialMode, Start board, remainingBoards)
            else game
        other -> handleEvent other game
      where
        (board, remainingBoards) = getBoards boards

-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = -- | Start screen.
    StartScreen
  | -- | Game is on with 'world' state.
    GameOn world

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

-- | Handle mouse clicks and key presses to manipulate game.
handleGame :: Event -> Game -> Game
handleGame (PointerPress mouse) (OpenCell, Start rawBoard) =
  -- Initialize board on first move.
  checkWin
    ( openCellWithNeighbors
        clickPos
        ( OpenCell,
          InProcess
            (minesToBoard (toBoard (zip (anticlockwiseCoords (fromCoords clickPos)) rawBoard)))
        )
    )
  where
    clickPos = pointToCoords mouse
handleGame (PointerPress mouse) game@(OpenCell, InProcess _) =
  -- Open cell on click.
  checkWin (openCellWithNeighbors (pointToCoords mouse) game)
handleGame (KeyPress "Ctrl") (_, state) = (MarkCell, state) -- Enable cell marking on 'Ctrl' press.
handleGame (KeyRelease "Ctrl") (_, state) = (OpenCell, state) -- Disable cell marking on 'Ctrl' release.
handleGame (PointerPress mouse) game@(MarkCell, state) =
  -- Mark cell on click.
  case state of
    (InProcess board) -> (MarkCell, InProcess (markCell (pointToCoords mouse) board))
    _ -> game
handleGame _ game = game

-- | Create a new game with StdGen.
-- For now infinite sequence of boards is used for multiple game sessions.
-- In the near future it will be replaced with encoded message.
initialState :: StdGen -> String -> MultiBoardGame
initialState stdGen message = (OpenCell, Start board, boards)
  where
    (board, boards) = getBoards (encodeMessage stdGen message)

-- | Default entry point.
run :: IO ()
run = do
  stdGen <- getStdGen
  let inputMessage = "A"
  (withMultipleGames . withStartingScreen) activityOf (initialState stdGen inputMessage) multiBoardHandle multiBoardDraw
  where
    -- Wrap 'handleGame' to make it works with 'MultiBoardGame' instead of 'Game'.
    multiBoardHandle event (mode, state, boards) = (newMode, newState, boards)
      where
        (newMode, newState) = handleGame event (mode, state)
    -- Wrap 'drawGame' to make it works with 'MultiBoardGame' instead of 'Game'.
    multiBoardDraw (_, Start [], []) = endScreen
    multiBoardDraw (mode, state, _) = drawGame (mode, state)

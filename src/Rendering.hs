{-# LANGUAGE OverloadedStrings #-}

module Rendering where

import CodeWorld
import Constants
import Data.Text (Text, pack)
import Datatype
import Utility

-- | Convert integer RGB color representation to the actual color.
intRGB :: Int -> Int -> Int -> Color
intRGB r g b = RGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

-- | Shortcut for square drawing.
solidSquare :: Double -> Picture
solidSquare size = solidRectangle size size

-- | Colors for different number of neighbors.
numberColor :: Int -> Color
numberColor 1 = blue
numberColor 2 = dark green
numberColor 3 = red
numberColor 4 = dark blue
numberColor 5 = dark red
numberColor 6 = dark (intRGB 127 255 212)
numberColor 7 = black
numberColor 8 = gray
numberColor _ = RGBA 0 0 0 0

-- | Red flag emoji.
flagMark :: Text
flagMark = "\x1F6A9"

-- | Question mark emoji.
questionMark :: Text
questionMark = "\x2753"

-- | Bomb emoji.
bombMark :: Text
bombMark = "\x1F4A3"

-- | Red cross emoji.
crossMark :: Text
crossMark = "\x274C"

-- | Color for buttons and etc.
baseColor :: Color
baseColor = intRGB 191 191 191

-- | Color for shadow parts of buttons.
shadowColor :: Color
shadowColor = intRGB 127 127 129

-- | Color for glare parts of buttons.
glareColor :: Color
glareColor = white

-- | Draw game of given state.
drawGame :: Game -> Picture
drawGame (_, state) =
  translated
    (- dx)
    (- dy)
    ( translated dx (2 * dy + messageMargin) (lettering (stateMessage state))
        <> pictures (map (drawCell state) (concat (enumerateBoard board)))
    )
  where
    (dx, dy) = shift
    stateMessage (Win _) = "You won"
    stateMessage (Lose _ _) = "You lose"
    stateMessage _ = ""
    messageMargin = 0.5
    board = extractBoard state

-- | Draw button at given coordinates and state (pressed or not).
drawButton :: (Double, Double) -> Bool -> Picture
drawButton (buttonWidth, buttonHeight) isNotPressed =
  colored
    baseColor
    (solidRectangle (buttonWidth - cellPadding * 2) (buttonHeight - cellPadding * 2))
    <> pictures (zipWith drawColoredPolygon [shadowColor, glareColor] [True, False])
  where
    vertices =
      [ (0, buttonHeight),
        (buttonHeight / 2, buttonHeight / 2),
        (buttonWidth - buttonHeight / 2, buttonHeight / 2),
        (buttonWidth, 0)
      ]
    getCornerCoords isShadow
      | isNotPressed == isShadow = (buttonWidth, buttonHeight)
      | otherwise = (0, 0)
    getCoords isShadow = getCornerCoords isShadow : vertices
    drawColoredPolygon color isShadow =
      translated
        (- cellSize / 2)
        (- cellSize / 2)
        (colored color (solidPolygon (getCoords isShadow)))

-- | Draw cell background without marks and content.
drawCellBackground :: CellState -> Color -> Picture
drawCellBackground Opened color =
  colored color (solidSquare (cellSize - cellPadding))
    <> colored shadowColor (solidSquare cellSize)
drawCellBackground _ _ = drawButton (cellSize, cellSize) True

-- | Draw cell content or mark depending on state.
drawCellContent :: CellContent -> CellState -> Picture
drawCellContent _ Flagged = drawLettering flagMark
drawCellContent _ Marked = drawLettering questionMark
drawCellContent Bomb Opened = drawLettering bombMark
drawCellContent (Neighbors (Just number)) Opened = colored (numberColor number) (drawLettering (pack (show number)))
drawCellContent _ _ = blank

-- | Draw text (emoji or number) in cell.
drawLettering :: Text -> Picture
drawLettering text = translated 0 (- cellPadding / 2) (scaled scaleFactor scaleFactor (styledLettering Bold Serif text))
  where
    scaleFactor = cellSize * ((cellSize - cellPadding * 3) / cellSize)

-- | Draw cell at given coordinates.
drawCell :: GameState -> (Coords, Cell) -> Picture
drawCell gameState (coords, Cell content state) =
  case gameState of
    (Lose _ lastMove) ->
      case (content, state) of
        (Bomb, _) -> drawBombOnLose (lastMove == coords) state
        (_, Flagged) -> drawIncorrectGuess
        (_, Marked) -> drawIncorrectGuess
        _ -> defaultDraw
    Win _ -> drawCellOnWin
    _ -> defaultDraw
  where
    (x, y) = fromCoords coords
    drawIncorrectGuess =
      moved
        ( drawLettering crossMark
            <> drawCellContent Bomb Opened
            <> drawCellBackground Opened baseColor
        )
    drawBombOnLose True _ = moved (drawCellContent Bomb Opened <> drawCellBackground Opened red)
    drawBombOnLose False Closed = moved (drawCellContent Bomb Opened <> drawCellBackground Opened baseColor)
    drawBombOnLose _ _ = defaultDraw

    defaultDraw =
      moved
        ( drawCellContent content state
            <> drawCellBackground state baseColor
        )

    moved =
      translated
        (x * cellSize)
        (y * cellSize)

    drawCellOnWin =
      case content of
        Bomb -> moved (drawCellContent Bomb Flagged <> drawCellBackground Flagged baseColor)
        _ -> defaultDraw

-- | Game start screen.
startingScreen :: Picture
startingScreen = scaled 2 2 (lettering "Minesweeper") <> translated 0 (-4) (lettering "[press SPACE to start]")

-- | Game end screen to inform player about the end of games sequence.
endScreen :: Picture
endScreen = scaled 2 2 (lettering "All available games exceeded")

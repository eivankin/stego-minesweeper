{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rendering where

import CodeWorld
import Data.Text (Text, pack)
import Datatype
import Utility
import Constants

intRGB :: Int -> Int -> Int -> Color
intRGB r g b = RGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

solidSquare :: Double -> Picture
solidSquare size = solidRectangle size size

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

flagMark :: Text
flagMark = "\x1F6A9"

questionMark :: Text
questionMark = "\x2753"

bombMark :: Text
bombMark = "\x1F4A3"

baseColor :: Color
baseColor = intRGB 191 191 191

shadowColor :: Color
shadowColor = intRGB 127 127 129

glareColor :: Color
glareColor = white

drawBoard :: Game -> Picture
drawBoard (state, board) = pictures (map drawCell (concat (enumerateBoard board)))

drawButton :: (Double, Double) -> Bool -> Picture
drawButton (width, height) isConvex =
  colored
    baseColor
    (solidSquare (width - cellPadding * 2))
    <> pictures (zipWith drawColoredPolygon [shadowColor, glareColor] [True, False])
  where
    vertices = [(0, height), (height / 2, height / 2), (width - height / 2, height / 2), (width, 0)]
    getCornerCoords isShadow
      | isConvex == isShadow = (width, height)
      | otherwise = (0, 0)
    getCoords isShadow = getCornerCoords isShadow : vertices
    drawColoredPolygon color isShadow =
      translated
        (- cellSize / 2)
        (- cellSize / 2)
        (colored color (solidPolygon (getCoords isShadow)))

drawCellBackground :: CellState -> Picture
drawCellBackground Opened =
  colored baseColor (solidSquare (cellSize - cellPadding))
    <> colored shadowColor (solidSquare cellSize)
drawCellBackground _ = drawButton (cellSize, cellSize) True

drawCellContent :: CellContent -> CellState -> Picture
drawCellContent _ Flagged = drawLettering flagMark
drawCellContent _ Marked = drawLettering questionMark
drawCellContent Bomb Opened = drawLettering bombMark
drawCellContent (Neighbors (Just number)) Opened = colored (numberColor number) (drawLettering (pack (show number)))
drawCellContent _ _ = blank

drawLettering :: Text -> Picture
drawLettering text = translated 0 (- cellPadding / 2) (scaled scaleFactor scaleFactor (styledLettering Bold Serif text))
  where
    scaleFactor = 1 / cellSize * ((cellSize - cellPadding * 3) / cellSize)

drawCell :: (Coords, Cell) -> Picture
drawCell (coords, Cell content state) =
  translated
    (x * cellSize)
    (y * cellSize)
    ( drawCellContent content state
        <> drawCellBackground state
    )
  where
    (x, y) = fromCoords coords

drawShifted :: Coords -> (a -> Picture) -> a -> Picture
drawShifted coords drawFunction arguments = translated shiftX shiftY (drawFunction arguments)
  where
    (shiftX, shiftY) = fromCoords coords

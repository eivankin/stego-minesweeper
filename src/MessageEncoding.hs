module MessageEncoding (encodeMessage) where

import Constants
import System.Random
import MessageUtils

encodeMessage :: StdGen -> String -> [[Bool]]
encodeMessage stdGen message = takeGroups binaryMessage
  where
    binaryMessage = toBinary message

    processGroup :: Bool -> [Bool] -> [Bool]
    processGroup isLast group =
      completeWithFalses
        boardSize
        (concat [[startingBit, continueBit], remainingMinesIndicator, group, remainingMines, [endingBit]])
      where
        boardSize = boardWidth * boardHeight
        startingBit = False
        continueBit = not isLast
        endingBit = isLast

        remainingCount = remainingMinesCount group
        remainingMinesIndicator = if isLast then numToBin indicatorLen remainingCount else []
        realRemainingCount = remainingCount - onesCount remainingMinesIndicator
        remainingMines = if isLast then fst (spanOnes realRemainingCount (randoms stdGen)) else []

    takeGroups :: [Bool] -> [[Bool]]
    takeGroups [] = []
    takeGroups msg = processGroup (null remaining) group : takeGroups remaining
      where
        (group, remaining) = spanOnes (numberOfMines - 1) msg

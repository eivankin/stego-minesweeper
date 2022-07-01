module MessageEncoding (encodeMessage, spanOnes, completeWithFalses, onesCount) where

import Constants
import Data.Char
import System.Random

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

completeWithFalses :: Int -> [Bool] -> [Bool]
completeWithFalses targetLen list = list ++ falses (targetLen - length list)

remainingMinesCount :: [Bool] -> Int
remainingMinesCount board = numberOfMines - 1 - onesCount board

falses :: Int -> [Bool]
falses count = map (const False) [0 .. count]

onesCount :: [Bool] -> Int
onesCount list = length (filter id list)

toBinary :: String -> [Bool]
toBinary = concatMap (numToBin 8 . ord)

numToBin :: Int -> Int -> [Bool]
numToBin bitsCount num = reverse (map (\p -> (num `div` (2 ^ p)) `mod` 2 == 1) [0 .. bitsCount - 1])

spanOnes :: (Ord a, Num a) => a -> [Bool] -> ([Bool], [Bool])
spanOnes _ [] = ([], [])
spanOnes neededOnes xs@(x : xs')
  | neededOnes > 0 = let (ys, zs) = spanOnes (if x then neededOnes - 1 else neededOnes) xs' in (x : ys, zs)
  | otherwise = ([], xs)

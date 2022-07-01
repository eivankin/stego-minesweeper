{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module MessageDecoding where

import Constants
import Data.Char
import Data.List.Split
import Data.Maybe (listToMaybe)
import MessageEncoding (completeWithFalses, onesCount, spanOnes)

decodeMessage :: [[Bool]] -> String
decodeMessage boards =
  map
    decodeChar
    ( filter
        nonEmpty
        ( map
            fixChar
            (bytes (concatMap (reverse . dropWhile not . reverse . drop 2) boardsExceptLast ++ processedLast))
        )
    )
  where
    (boardsExceptLast, lastBoard : _) = span isNotLast boards
    isNotLast (_ : s : _) = s
    isNotLast _ = False
    
    nonEmpty list = not (null list)
    
    decodeChar = chr . binToInt

    fixChar binary
      | length binary == 8 = binary
      | otherwise =
        case takeLast binary of
          Just True -> completeWithFalses 8 binary
          _ -> []

    bytes = chunksOf 8

    remainingMinesIndicator = take indicatorLen (drop 2 lastBoard)
    skipFromEnd = binToInt remainingMinesIndicator - onesCount remainingMinesIndicator + 1
    processedLast = reverse (snd (spanOnes skipFromEnd (reverse (drop (2 + indicatorLen) lastBoard))))

binToInt :: [Bool] -> Int
binToInt list = sum (zipWith (\b p -> if b then 2 ^ p else 0) (reverse list) [0 ..])

takeLast :: [a] -> Maybe a
takeLast list = listToMaybe (take 1 (reverse list))

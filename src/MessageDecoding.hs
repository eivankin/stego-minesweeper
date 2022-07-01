module MessageDecoding where

import Constants
import Data.Char
import Data.List.Split
import MessageUtils

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
    (boardsExceptLast, lastBoards) = span isNotLast boards
    lastBoard = 
      case lastBoards of
        [board] -> board
        _ -> error "Input is invalid: last board not found"
    
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

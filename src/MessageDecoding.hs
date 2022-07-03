module MessageDecoding where

import Constants
import Data.Char
import Data.List.Split
import MessageUtils

-- | Decode message from given sequence of boolean arrays.
-- Each array represents a board from game.
decodeMessage :: [[Bool]] -> String
decodeMessage boards =
  map
    decodeChar
    ( filter
        nonEmpty
        ( map
            fixChar
            ( bytes
                ( concatMap processBoard boardsExceptLast
                    ++ processedLast
                )
            )
        )
    )
  where
    (boardsExceptLast, lastBoards) = span isNotLast boards
    lastBoard =
      case lastBoards of
        [board] -> board
        _ -> error "Input is invalid: last board not found"

    isNotLast (_ : s : _) = s -- Check continue bit to determine whether the given board is last
    isNotLast _ = False

    nonEmpty list = not (null list)

    decodeChar = chr . binToInt

    processBoard = -- Remove empty ending bits and info (continue bit)
      reverse . dropWhile not . reverse . drop 2

    fixChar binary
      | length binary == 8 = binary
      | otherwise =
        case takeLast binary of
          Just True -> completeWithFalses 8 binary
          _ -> []

    bytes = chunksOf 8

    remainingMinesIndicator = take indicatorLen (drop 2 lastBoard)
    skipFromEnd =
      -- How many random mines skip from end of the last board
      binToInt remainingMinesIndicator - onesCount remainingMinesIndicator + 1
    processedLast =
      -- Remove random and info (ending bit, continue bit, binary number of additional mines) bits
      reverse (snd (spanOnes skipFromEnd (reverse (drop (2 + indicatorLen) lastBoard))))

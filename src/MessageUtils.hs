{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module MessageUtils where

import Constants
import Data.Char
import Data.Maybe (listToMaybe)

-- | Create a list of 'False' of given length. 
falses :: Int -> [Bool]
falses count = map (const False) [0 .. count]

-- | Count the number of 'True' in the list.
onesCount :: [Bool] -> Int
onesCount list = length (filter id list)

-- | Convert ASCII string to an boolean list.
toBinary :: String -> [Bool]
toBinary = concatMap (numToBin 8 . ord)

-- | Convert number to its binary representation with given number of bits.
numToBin :: Int -> Int -> [Bool]
numToBin bitsCount num = reverse (map (\p -> (num `div` (2 ^ p)) `mod` 2 == 1) [0 .. bitsCount - 1])

-- | Split boolean list into two parts: with given number of 'True's and remaining part. 
spanOnes :: (Ord a, Num a) => a -> [Bool] -> ([Bool], [Bool])
spanOnes _ [] = ([], [])
spanOnes neededOnes xs@(x : xs')
  | neededOnes > 0 = let (ys, zs) = spanOnes (if x then neededOnes - 1 else neededOnes) xs' in (x : ys, zs)
  | otherwise = ([], xs)

-- | Convert binary representation of some number to the number itself.
binToInt :: [Bool] -> Int
binToInt list = sum (zipWith (\b p -> if b then 2 ^ p else 0) (reverse list) [0 ..])

-- | Safely take the last element of a finite list.
takeLast :: [a] -> Maybe a
takeLast list = listToMaybe (take 1 (reverse list))

-- | Complete given boolean list to reach the given length with 'False's. 
completeWithFalses :: Int -> [Bool] -> [Bool]
completeWithFalses targetLen list = list ++ falses (targetLen - length list)

-- | Get the number of mines to complete the given board.
remainingMinesCount :: [Bool] -> Int
remainingMinesCount board = numberOfMines - 1 - onesCount board

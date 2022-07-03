{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module MessageUtils where

import Constants
import Data.Char
import Data.Maybe (listToMaybe)

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

binToInt :: [Bool] -> Int
binToInt list = sum (zipWith (\b p -> if b then 2 ^ p else 0) (reverse list) [0 ..])

takeLast :: [a] -> Maybe a
takeLast list = listToMaybe (take 1 (reverse list))

completeWithFalses :: Int -> [Bool] -> [Bool]
completeWithFalses targetLen list = list ++ falses (targetLen - length list)

remainingMinesCount :: [Bool] -> Int
remainingMinesCount board = numberOfMines - 1 - onesCount board

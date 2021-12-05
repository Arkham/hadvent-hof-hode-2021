{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (digitToInt)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let entries = map parseLine (T.lines contents)
  let gammaRate = map (findMostCommon 0) (transpose entries)
  let epsilonRate = map complement gammaRate
  print $ binaryDigitsToInt gammaRate * binaryDigitsToInt epsilonRate
  let oxygenRating = findAllSelected (findMostCommon 1) entries []
  let co2Rating = findAllSelected (complement . findMostCommon 1) entries []
  print $ binaryDigitsToInt oxygenRating * binaryDigitsToInt co2Rating

parseLine :: T.Text -> [Int]
parseLine input =
  map digitToInt $ T.unpack input

findMostCommon :: Int -> [Int] -> Int
findMostCommon tieBreaker bits =
  let (zeroes, ones) =
        foldl
          ( \(zeroes, ones) elem ->
              case elem of
                0 ->
                  (zeroes + 1, ones)
                1 ->
                  (zeroes, ones + 1)
          )
          (0, 0)
          bits
   in case compare zeroes ones of
        GT ->
          0
        LT ->
          1
        EQ ->
          tieBreaker

complement :: Int -> Int
complement 0 = 1
complement 1 = 0

binaryDigitsToInt :: [Int] -> Int
binaryDigitsToInt =
  foldl (\acc el -> acc * 2 + el) 0

findAllSelected :: ([Int] -> Int) -> [[Int]] -> [Int] -> [Int]
findAllSelected selectValue entries acc =
  case (entries, transpose entries) of
    ([one], _) ->
      reverse acc <> one
    (_, []) ->
      reverse acc
    (_, first : _) ->
      let selectedValue = selectValue first
          nextBatch =
            map (drop 1) $
              filter (\entry -> head entry == selectedValue) entries
       in findAllSelected selectValue nextBatch (selectedValue : acc)

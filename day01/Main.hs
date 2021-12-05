{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let measurements = map parseLine (T.lines contents)
  let slidingWindows =
        map (\(a, b, c) -> a + b + c) $
          zip3 measurements (drop 1 measurements) (drop 2 measurements)
  print $ countIncreases slidingWindows

parseLine :: T.Text -> Integer
parseLine = read . T.unpack

countIncreases :: [Integer] -> Int
countIncreases measurements =
  let pairs = zip measurements (drop 1 measurements)
   in length $ filter (uncurry (<)) pairs

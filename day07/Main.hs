{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let positions = map toInt $ T.splitOn "," contents
  let positionWeights =
        foldl
          ( flip $
              HM.alter
                ( \case
                    Nothing -> Just 1
                    Just v -> Just $ v + 1
                )
          )
          HM.empty
          positions
  print $ solveWith id positionWeights
  print $ solveWith gaussSum positionWeights

rangeOfValues :: HM.HashMap Int Int -> [Int]
rangeOfValues acc =
  let keys = HM.keys acc
   in [minimum keys .. maximum keys]

solveWith :: (Int -> Int) -> HM.HashMap Int Int -> (Int, Int)
solveWith solveFun positionWeights =
  minimum $
    map
      ( \elem ->
          ( HM.foldlWithKey
              (\acc pos count -> acc + solveFun (abs (elem - pos)) * count)
              0
              positionWeights,
            elem
          )
      )
      (rangeOfValues positionWeights)

-- each step goes 1,2,3,4,5..
-- we basically need to sum the first N natural numbers
-- https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
gaussSum :: Int -> Int
gaussSum n =
  n * (n + 1) `div` 2

toInt :: T.Text -> Int
toInt =
  read . T.unpack

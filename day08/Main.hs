{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "sample.txt"
  let inputs = mapMaybe parseLine (T.lines contents)
  let results = map (uncurry deduceOutput) inputs
  let simpleDigits = Set.fromList [1, 4, 7, 8]
  print $ length $ filter (`Set.member` simpleDigits) $ List.concat results
  print $ List.sum (map (List.foldl (\acc e -> acc * 10 + e) 0) results)

parseLine :: T.Text -> Maybe ([Set.Set Char], [Set.Set Char])
parseLine input =
  case T.splitOn " | " input of
    [signalPatterns, outputValue] ->
      Just
        ( intoSets signalPatterns,
          intoSets outputValue
        )
    _ ->
      Nothing
  where
    intoSets str = map (Set.fromList . T.unpack) $ T.splitOn " " str

deduceOutput :: [Set.Set Char] -> [Set.Set Char] -> [Int]
deduceOutput signals outputs =
  let s1 = head $ findWithLength signals 2
      s4 = head $ findWithLength signals 4
      s7 = head $ findWithLength signals 3
      s8 = head $ findWithLength signals 7
      s3 = head $ List.filter (Set.isSubsetOf s1) $ findWithLength signals 5
      s9 = head $ List.filter (Set.isSubsetOf s3) $ findWithLength signals 6
      s0 = head $ List.filter (\e -> e /= s9 && Set.isSubsetOf s7 e) $ findWithLength signals 6
      s6 = head $ List.filter (\e -> e /= s9 && e /= s0) $ findWithLength signals 6
      s5 = head $ List.filter (`Set.isSubsetOf` s6) $ findWithLength signals 5
      s2 = head $ List.filter (\e -> e /= s3 && e /= s5) $ findWithLength signals 5
      mappings =
        [ (s0, 0),
          (s1, 1),
          (s2, 2),
          (s3, 3),
          (s4, 4),
          (s5, 5),
          (s6, 6),
          (s7, 7),
          (s8, 8),
          (s9, 9)
        ]
   in map (\e -> snd $ head $ List.filter (\(k, v) -> k == e) mappings) outputs

findWithLength :: [Set.Set Char] -> Int -> [Set.Set Char]
findWithLength signals size =
  List.filter (\e -> length e == size) signals

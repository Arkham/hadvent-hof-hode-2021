{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.List.Split as Split
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import GHC.Generics (Generic)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case T.splitOn "\n\n" contents of
    [initialInput, rulesInput] -> do
      let initial = T.unpack initialInput
      let rules = mapMaybe parseLine (T.lines rulesInput)
      let rulesMap = HM.fromList rules
      -- part I
      let final =
            foldl
              (\acc _ -> evolve rulesMap acc)
              initial
              [1 .. 10]
      let counts = HM.elems $ frequencies final
      print $ maximum counts - minimum counts
      -- part II
      -- Instead of keeping a full string, represent it
      -- as a map of a pair of characters pointing to a count.
      -- Note that we have to treat the starting pair differently
      -- in order to count the characters in the string. Example:
      -- > ABABA
      -- Start  (A, B) : 1 -> 1 A, 1 B
      -- Normal (B, A) : 2 -> 2 A
      -- Normal (A, B) : 1 -> 1 B
      -- In total          -> 3 A, 2 B
      let initialPairs = zip initial (drop 1 initial)
      let counter = buildCounter initialPairs
      let finalCounter =
            foldl
              (\acc _ -> evolveCounter rulesMap acc)
              counter
              [1 .. 40]
      let newCounts = HM.elems $ counterFrequencies finalCounter
      print $ maximum newCounts - minimum newCounts
    _ ->
      putStrLn "could not parse file"

data Pair
  = Start (Char, Char)
  | Normal (Char, Char)
  deriving (Eq, Generic, Show)

instance Hashable Pair

buildCounter :: [(Char, Char)] -> HM.HashMap Pair Int
buildCounter [] = HM.empty
buildCounter (first : rest) =
  go rest $ HM.singleton (Start first) 1
  where
    go [] acc = acc
    go (x : xs) acc =
      go xs $ increase 1 (Normal x) acc

frequencies :: String -> HM.HashMap Char Int
frequencies =
  foldl (flip $ increase 1) HM.empty

parseLine :: T.Text -> Maybe ((Char, Char), Char)
parseLine input =
  case Split.splitOn " -> " (T.unpack input) of
    [[first, second], [toInsert]] ->
      Just ((first, second), toInsert)
    _ ->
      Nothing

evolve :: HM.HashMap (Char, Char) Char -> String -> String
evolve rulesMap input@(firstInput : _) =
  go pairs [firstInput]
  where
    pairs = zip input (drop 1 input)
    go [] acc = reverse acc
    go (first@(pairFirst, pairSecond) : rest) acc =
      case HM.lookup first rulesMap of
        Just replacement ->
          go rest (pairSecond : replacement : acc)
        Nothing ->
          go rest (pairSecond : acc)

evolveCounter ::
  HM.HashMap (Char, Char) Char ->
  HM.HashMap Pair Int ->
  HM.HashMap Pair Int
evolveCounter rulesMap counter =
  go counts []
  where
    counts = HM.toList counter

    go [] acc = HM.fromListWith (+) acc
    go (first : rest) acc =
      go rest $
        case first of
          (Start pair@(f, s), 1) ->
            let toInsert = rulesMap ! pair
             in ((Start (f, toInsert), 1) : (Normal (toInsert, s), 1) : acc)
          (Normal pair@(f, s), count) ->
            let toInsert = rulesMap ! pair
             in ((Normal (f, toInsert), count) : (Normal (toInsert, s), count) : acc)

counterFrequencies :: HM.HashMap Pair Int -> HM.HashMap Char Int
counterFrequencies =
  HM.foldlWithKey
    ( \acc k v ->
        case (k, v) of
          (Start (firstChar, secondChar), 1) ->
            increase 1 firstChar $
              increase 1 secondChar acc
          (Normal (_, secondChar), count) ->
            increase count secondChar acc
    )
    HM.empty

increase ::
  (Eq a, Hashable a) =>
  Int ->
  a ->
  HM.HashMap a Int ->
  HM.HashMap a Int
increase count k =
  HM.insertWith (+) k count

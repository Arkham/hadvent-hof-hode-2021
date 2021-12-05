{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

type Point = (Int, Int)

type Vent = (Point, Point)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let vents = mapMaybe parseLine (T.lines contents)
  let onlyHorizAndVert = filter isHorizOrVert vents
  let filled = foldl fillVents HM.empty onlyHorizAndVert
  print $ length $ filter (\(_, b) -> b >= 2) $ HM.toList filled
  let allFilled = foldl fillVents HM.empty vents
  print $ length $ filter (\(_, b) -> b >= 2) $ HM.toList allFilled

parseLine :: T.Text -> Maybe Vent
parseLine input =
  case T.splitOn " -> " input of
    [first, second] ->
      (,) <$> parsePoint first <*> parsePoint second
    _ ->
      Nothing

parsePoint :: T.Text -> Maybe Point
parsePoint input =
  case T.splitOn "," input of
    [first, second] ->
      Just (toInt first, toInt second)
    _ ->
      Nothing

toInt :: T.Text -> Int
toInt = read . T.unpack

isHorizOrVert :: Vent -> Bool
isHorizOrVert ((x1, y1), (x2, y2)) =
  x1 == x2 || y1 == y2

coveredPoints :: Vent -> [Point]
coveredPoints vent@((x1, y1), (x2, y2)) =
  let run = x2 - x1
      rise = y2 - y1
      stepX = calcStep run
      stepY = calcStep rise
   in findPoints vent (stepX, stepY)

calcStep :: Int -> Int
calcStep delta
  | delta > 0 = 1
  | delta == 0 = 0
  | delta < 0 = -1

findPoints :: Vent -> (Int, Int) -> [Point]
findPoints ((x1, y1), (x2, y2)) (stepX, stepY) =
  go [(x1, y1)]
  where
    go acc@((firstX, firstY) : _) =
      if firstX == x2 && firstY == y2
        then acc
        else go ((firstX + stepX, firstY + stepY) : acc)

fillVents :: HM.HashMap Point Int -> Vent -> HM.HashMap Point Int
fillVents acc vent =
  let inBetween = coveredPoints vent
   in foldl
        ( flip $
            HM.alter
              ( \case
                  Nothing -> Just 1
                  Just v -> Just (v + 1)
              )
        )
        acc
        inBetween

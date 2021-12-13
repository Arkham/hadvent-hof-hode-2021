{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case T.splitOn "\n\n" contents of
    [dotsInput, foldsInput] -> do
      let dots = mapMaybe parseDot (T.lines dotsInput)
      let folds = mapMaybe parseFold (T.lines foldsInput)
      let maxX = maximum $ map fst dots
      let maxY = maximum $ map snd dots
      let dotSet = Set.fromList dots
      let initial = (dotSet, (maxX, maxY))
      print $ length $ fst $ applyFold initial (head folds)
      let final = foldl applyFold initial folds
      putStrLn $ render final
    _ ->
      putStrLn "could not parse file"

applyFold ::
  (Set.Set (Int, Int), (Int, Int)) ->
  Fold ->
  (Set.Set (Int, Int), (Int, Int))
applyFold (initial, (maxX, maxY)) (AlongY yFold) =
  ( Set.map
      ( \(x, y) ->
          if y < yFold
            then (x, y)
            else (x, maxY - y)
      )
      initial,
    (maxX, yFold - 1)
  )
applyFold (initial, (maxX, maxY)) (AlongX xFold) =
  ( Set.map
      ( \(x, y) ->
          if x < xFold
            then (x, y)
            else (maxX - x, y)
      )
      initial,
    (xFold - 1, maxY)
  )

render :: (Set.Set (Int, Int), (Int, Int)) -> String
render (set, (maxX, maxY)) =
  List.intercalate "\n" $
    map
      ( \yIndex ->
          List.intercalate "" $
            map
              ( \xIndex ->
                  if Set.member (xIndex, yIndex) set
                    then "#"
                    else "."
              )
              [0 .. maxX]
      )
      [0 .. maxY]

parseDot :: T.Text -> Maybe (Int, Int)
parseDot input =
  case T.splitOn "," input of
    [x, y] ->
      Just (toInt x, toInt y)
    _ ->
      Nothing

data Fold
  = AlongX Int
  | AlongY Int
  deriving (Show)

parseFold :: T.Text -> Maybe Fold
parseFold input =
  case ( T.splitOn "fold along x=" input,
         T.splitOn "fold along y=" input
       ) of
    (["", x], _) ->
      Just $ AlongX $ toInt x
    (_, ["", y]) ->
      Just $ AlongY $ toInt y
    _ ->
      Nothing

toInt :: T.Text -> Int
toInt = read . T.unpack

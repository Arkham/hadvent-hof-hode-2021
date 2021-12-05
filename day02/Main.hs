{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

data Move
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let moves = mapMaybe parseMove $ T.lines contents
  let initialPos = Position 0 0 0
  let Position {horizontal, depth} = foldl makeMove initialPos moves
  print $ horizontal * depth

parseMove :: T.Text -> Maybe Move
parseMove input =
  case T.splitOn " " input of
    ["forward", val] ->
      Just $ Forward (toInt val)
    ["down", val] ->
      Just $ Down (toInt val)
    ["up", val] ->
      Just $ Up (toInt val)
    _ ->
      Nothing

toInt :: T.Text -> Int
toInt = read . T.unpack

makeMove :: Position -> Move -> Position
makeMove pos@Position {horizontal, depth, aim} move =
  case move of
    Forward v ->
      pos {horizontal = horizontal + v, depth = depth + (aim * v)}
    Down v ->
      pos {aim = aim + v}
    Up v ->
      pos {aim = aim - v}

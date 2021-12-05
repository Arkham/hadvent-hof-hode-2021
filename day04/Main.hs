{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let (numbersInput : boardsInput) =
        Split.splitWhen (== "") $ T.lines contents
  let drawnNumbers = map toInt $ T.splitOn "," (head numbersInput)
  let boards = map parseBoard boardsInput
  let (lastDrawn, winningBoard) = findWinningBoard drawnNumbers boards
  print $ lastDrawn * findUnmarkedSum winningBoard
  case findLastWinningBoard drawnNumbers boards Nothing of
    Nothing ->
      putStrLn "no one won"
    Just (lastDrawn, winningBoard) ->
      print $ lastDrawn * findUnmarkedSum winningBoard

toInt :: T.Text -> Int
toInt = read . T.unpack

data Tile
  = Marked Int
  | Unmarked Int
  deriving (Show)

isMarked :: Tile -> Bool
isMarked (Marked _) = True
isMarked _ = False

type Board = [[Tile]]

parseBoard :: [T.Text] -> Board
parseBoard =
  map (map (Unmarked . toInt) . T.words)

findWinningBoard :: [Int] -> [Board] -> (Int, Board)
findWinningBoard (drawn : rest) boards =
  let newBoards = map (mark drawn) boards
   in case List.find isWinningBoard newBoards of
        Nothing ->
          findWinningBoard rest newBoards
        Just board ->
          (drawn, board)

findLastWinningBoard :: [Int] -> [Board] -> Maybe (Int, Board) -> Maybe (Int, Board)
findLastWinningBoard [] boards acc = acc
findLastWinningBoard (drawn : rest) boards acc =
  let newBoards = map (mark drawn) boards
   in case List.partition isWinningBoard newBoards of
        ([], _) ->
          findLastWinningBoard rest newBoards acc
        (first : _, notWinners) ->
          findLastWinningBoard rest notWinners (Just (drawn, first))

mark :: Int -> Board -> Board
mark num =
  map (map (markTile num))

markTile :: Int -> Tile -> Tile
markTile num tile =
  case tile of
    Unmarked current ->
      if current == num
        then Marked num
        else tile
    _ ->
      tile

isWinningBoard :: Board -> Bool
isWinningBoard board =
  anyFullyMarked board || anyFullyMarked (List.transpose board)
  where
    anyFullyMarked b = List.any (List.all isMarked) b

findUnmarkedSum :: Board -> Int
findUnmarkedSum board =
  List.sum $
    map
      ( List.sum
          . map
            ( \case
                Marked _ ->
                  0
                Unmarked v ->
                  v
            )
      )
      board

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Matrix (Matrix, (<->), (<|>))
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Debug.Trace

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let grid = Matrix.fromLists $ map parseLine (T.lines contents)
  print $ findLowestPath (1, 1) grid
  let largeGrid = makeLarge grid
  print $ findLowestPath (1, 1) largeGrid

type Pos = (Int, Int)

type Grid = Matrix Int

parseLine :: T.Text -> [Int]
parseLine = map (read . T.unpack) . T.chunksOf 1

offsets :: [(Int, Int)]
offsets =
  [(-1, 0), (1, 0), (0, -1), (0, 1)]

findLowestPath :: Pos -> Grid -> Maybe Int
findLowestPath start grid =
  HM.lookup end $ go Set.empty (Set.singleton (0, start)) (HM.singleton start 0)
  where
    end = (Matrix.nrows grid, Matrix.ncols grid)

    go visited queue distances
      | Set.null queue = distances
      | otherwise =
        let ((currentValue, currentPos), queueRest) = Set.deleteFindMin queue
            neighbours = findNeighbours currentValue currentPos visited grid

            shorter =
              filter
                ( \(val, pos) ->
                    case HM.lookup pos distances of
                      Nothing ->
                        True
                      Just v ->
                        val < v
                )
                neighbours

            (newQueue, newDistances) =
              foldl
                ( \(accQueue, accDistances) (val, pos) ->
                    ( Set.insert (val, pos) accQueue,
                      HM.insert pos val accDistances
                    )
                )
                (queueRest, distances)
                shorter
         in go (Set.insert currentPos visited) newQueue newDistances

findNeighbours :: Int -> Pos -> Set.Set Pos -> Grid -> [(Int, Pos)]
findNeighbours currentValue (x, y) visited grid =
  mapMaybe
    ( \(dx, dy) ->
        let newX = x + dx
            newY = y + dy
         in if Set.member (newX, newY) visited
              then Nothing
              else
                (\val -> (currentValue + val, (newX, newY)))
                  <$> Matrix.safeGet newX newY grid
    )
    offsets

makeLarge :: Grid -> Grid
makeLarge initial =
  let template =
        [ [0, 1, 2, 3, 4],
          [1, 2, 3, 4, 5],
          [2, 3, 4, 5, 6],
          [3, 4, 5, 6, 7],
          [4, 5, 6, 7, 8]
        ]
      increaseBy count grid =
        ( \v ->
            mod (v - 1 + count) 9 + 1
        )
          <$> grid

      nrows = Matrix.nrows initial
      ncols = Matrix.ncols initial
   in foldl
        ( \acc row ->
            acc
              <-> foldl
                ( \innerAcc count ->
                    innerAcc <|> increaseBy count initial
                )
                (Matrix.fromList nrows 0 [])
                row
        )
        (Matrix.fromList 0 (ncols * 5) [])
        template

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let grid = Matrix.fromLists $ map parseLine (T.lines contents)
  let localMinimums =
        catMaybes $
          Matrix.toList $
            Matrix.mapPos
              (isLocalMinimum grid)
              grid
  print $ sum $ map (\(val, _) -> val + 1) localMinimums

  let basinSizes = map (findBasinSize grid) localMinimums
  print $ product $ take 3 $ reverse $ List.sort basinSizes

isLocalMinimum :: Matrix.Matrix Int -> (Int, Int) -> Int -> Maybe (Int, (Int, Int))
isLocalMinimum matrix (x, y) val =
  let neighbours =
        mapMaybe
          (\(i, j) -> Matrix.safeGet (x + i) (y + j) matrix)
          offsets
   in if List.all (> val) neighbours
        then Just (val, (x, y))
        else Nothing

-- A basin is all locations that eventually flow downward to a single low
-- point. Therefore, every low point has a basin, although some basins are very
-- small. Locations of height 9 do not count as being in any basin, and all
-- other locations will always be part of exactly one basin.

-- The size of a basin is the number of locations within the basin, including
-- the low point.
findBasinSize :: Matrix.Matrix Int -> (Int, (Int, Int)) -> Int
findBasinSize matrix valWithPos =
  length $ doIt valWithPos
  where
    doIt (v, (x, y)) =
      let neighbours =
            mapMaybe
              ( \(i, j) ->
                  let newX = x + i
                      newY = y + j
                   in Matrix.safeGet newX newY matrix
                        >>= ( \n ->
                                if n > v && n /= 9
                                  then Just (n, (newX, newY))
                                  else Nothing
                            )
              )
              offsets
       in case neighbours of
            [] ->
              Set.singleton (x, y)
            other ->
              Set.unions (Set.singleton (x, y) : map doIt other)

offsets :: [(Int, Int)]
offsets =
  [(0, -1), (-1, 0), (1, 0), (0, 1)]

parseLine :: T.Text -> [Int]
parseLine =
  map toInt . T.chunksOf 1

toInt :: T.Text -> Int
toInt = read . T.unpack

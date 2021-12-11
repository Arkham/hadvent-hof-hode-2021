{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Debug.Trace

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let initialGrid = Matrix.fromLists $ map parseLine (T.lines contents)
  print $
    snd $
      foldl
        ( \(grid, sum) _ ->
            let newGrid = evolve grid
             in (newGrid, sum + countJustFlashed newGrid)
        )
        (initialGrid, 0)
        [1 .. 100]

  print $
    foldM
      ( \grid index ->
          let newGrid = evolve grid
           in if countJustFlashed newGrid == 100
                then Left index
                else Right newGrid
      )
      initialGrid
      [1 ..]

countJustFlashed :: Matrix.Matrix Octopus -> Int
countJustFlashed grid =
  length $ List.filter (== Unflashed 0) $ Matrix.toList grid

toInt :: T.Text -> Int
toInt = read . T.unpack

parseLine :: T.Text -> [Octopus]
parseLine = map (Unflashed . toInt) . T.chunksOf 1

printGrid :: Matrix.Matrix Octopus -> IO ()
printGrid grid = do
  putStrLn ""
  putStrLn $
    List.intercalate "\n" $
      map (List.intercalate "" . map showEl) $
        Matrix.toLists grid
  where
    showEl (Unflashed v) = show v
    showEl Flashed = "0"

data Octopus
  = Unflashed Int
  | Flashing
  | Flashed
  deriving (Eq, Show)

evolveOctopus :: Octopus -> Octopus
evolveOctopus (Unflashed 9) = Flashing
evolveOctopus (Unflashed n) = Unflashed $ n + 1
evolveOctopus Flashing = Flashed
evolveOctopus Flashed = Flashed

evolve :: Matrix.Matrix Octopus -> Matrix.Matrix Octopus
evolve initial =
  let increased = fmap evolveOctopus initial
      newFlashing =
        Maybe.catMaybes $
          Matrix.toList $
            Matrix.mapPos
              ( \coords val ->
                  if val == Flashing
                    then Just coords
                    else Nothing
              )
              increased
   in keepFlashing increased newFlashing

keepFlashing :: Matrix.Matrix Octopus -> [(Int, Int)] -> Matrix.Matrix Octopus
keepFlashing acc [] =
  fmap
    ( \case
        Flashed -> Unflashed 0
        Unflashed n -> Unflashed n
    )
    acc
keepFlashing acc (firstFlashing : restFlashing) =
  let (newAcc, newFlashing) =
        foldl
          ( \(acc, flashingAcc) (neighbour, neighbourPos) ->
              let evolvedNeighbour = evolveOctopus neighbour
               in ( Matrix.setElem evolvedNeighbour neighbourPos acc,
                    if evolvedNeighbour == Flashing
                      then neighbourPos : flashingAcc
                      else flashingAcc
                  )
          )
          ( Matrix.setElem Flashed firstFlashing acc,
            restFlashing
          )
          (neighboursWithPos acc firstFlashing)
   in keepFlashing newAcc newFlashing

neighboursWithPos :: Matrix.Matrix Octopus -> (Int, Int) -> [(Octopus, (Int, Int))]
neighboursWithPos grid (posX, posY) =
  Maybe.mapMaybe
    ( \(dx, dy) ->
        let newX = (posX + dx)
            newY = (posY + dy)
         in Matrix.safeGet newX newY grid
              >>= (\val -> Just (val, (newX, newY)))
    )
    offsets

offsets :: [(Int, Int)]
offsets =
  [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

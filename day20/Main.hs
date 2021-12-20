{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case T.splitOn "\n\n" contents of
    [first, second] ->
      do
        let algorithm = parseLine first
        let inputImage = buildImage $ map parseLine (T.lines second)
        let flippingPattern = [0, head algorithm]
        print $
          length $
            filter (== 1) $
              HM.elems $
                foldl
                  ( \acc (_, defaultValue) ->
                      evolve defaultValue algorithm acc
                  )
                  inputImage
                  (zip [1 .. 50] $ cycle flippingPattern)
    _ ->
      print "could not parse input"

evolve :: Int -> [Int] -> HM.HashMap (Int, Int) Int -> HM.HashMap (Int, Int) Int
evolve defaultValue algorithm input =
  let (ys, xs) = unzip $ HM.keys input
      minY = minimum ys
      maxY = maximum ys
      minX = minimum xs
      maxX = maximum xs
   in foldl
        ( \acc y ->
            foldl
              ( \innerAcc x ->
                  HM.insert (y, x) (getValue defaultValue (y, x) algorithm input) innerAcc
              )
              acc
              [(minX -1) .. (maxX + 1)]
        )
        HM.empty
        [(minY -1) .. (maxY + 1)]

getValue :: Int -> (Int, Int) -> [Int] -> HM.HashMap (Int, Int) Int -> Int
getValue defaultValue (y, x) algorithm input =
  let index =
        binaryToInt $
          map
            ( \(dy, dx) ->
                HM.lookupDefault defaultValue (y + dy, x + dx) input
            )
            offsets
   in algorithm !! index

offsets :: [(Int, Int)]
offsets =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 0),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

binaryToInt :: [Int] -> Int
binaryToInt =
  foldl (\acc el -> acc * 2 + el) 0

parseLine :: T.Text -> [Int]
parseLine input =
  map
    ( \case
        "#" -> 1
        "." -> 0
        _ -> undefined
    )
    (T.chunksOf 1 input)

buildImage :: [[Int]] -> HM.HashMap (Int, Int) Int
buildImage inputs =
  foldl
    ( \acc (row, rowIndex) ->
        foldl
          ( \innerAcc (el, elIndex) ->
              HM.insert (rowIndex, elIndex) el innerAcc
          )
          acc
          (zip row [0 ..])
    )
    HM.empty
    (zip inputs [0 ..])

renderImage :: HM.HashMap (Int, Int) Int -> String
renderImage image =
  let (ys, xs) = unzip $ HM.keys image
      minY = minimum ys
      maxY = maximum ys
      minX = minimum xs
      maxX = maximum xs
   in List.intercalate "\n" $
        map
          ( \y ->
              map
                ( \x ->
                    case HM.lookup (y, x) image of
                      Just 1 -> '#'
                      _ -> '.'
                )
                [minX .. maxX]
          )
          [minY .. maxY]

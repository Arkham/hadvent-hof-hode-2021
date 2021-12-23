{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Either (rights)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let instructions = rights $ map (P.parse instructionParser "") (T.lines contents)
  -- part 1
  print $
    length $
      foldl
        ( \acc (instruction, cube@(Cube (minX, maxX) (minY, maxY) (minZ, maxZ))) ->
            let pointSet = Set.fromList [(x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ]]
             in case (abs minX <= 50, instruction) of
                  (True, On) ->
                    Set.union acc pointSet
                  (True, Off) ->
                    Set.difference acc pointSet
                  _ ->
                    trace (show cube) acc
        )
        Set.empty
        instructions
  -- part 2
  print $ sum $ map score $ explode instructions

score :: (Instruction, Cube) -> Int
score (op, Cube (minX, maxX) (minY, maxY) (minZ, maxZ)) =
  factor op
    * (maxX - minX + 1)
    * (maxY - minY + 1)
    * (maxZ - minZ + 1)
  where
    factor On = 1
    factor Off = -1

data Cube
  = Cube (Int, Int) (Int, Int) (Int, Int)
  deriving (Show)

explode :: [(Instruction, Cube)] -> [(Instruction, Cube)]
explode instructions =
  go instructions []
  where
    go [] acc = acc
    go ((On, first) : rest) acc =
      go rest $ acc <> ((On, first) : mapMaybe (\(op, el) -> (flipInstruction op,) <$> intersect first el) acc)
    go ((Off, first) : rest) acc =
      go rest $ acc <> mapMaybe (\(op, el) -> (flipInstruction op,) <$> intersect first el) acc

intersect :: Cube -> Cube -> Maybe Cube
intersect (Cube (minX1, maxX1) (minY1, maxY1) (minZ1, maxZ1)) (Cube (minX2, maxX2) (minY2, maxY2) (minZ2, maxZ2))
  | minX1 > maxX2 = Nothing
  | minX2 > maxX1 = Nothing
  | minY1 > maxY2 = Nothing
  | minY2 > maxY1 = Nothing
  | minZ1 > maxZ2 = Nothing
  | minZ2 > maxZ1 = Nothing
  | otherwise =
    Just $
      Cube
        (max minX1 minX2, min maxX1 maxX2)
        (max minY1 minY2, min maxY1 maxY2)
        (max minZ1 minZ2, min maxZ1 maxZ2)

data Instruction
  = On
  | Off
  deriving (Show)

flipInstruction :: Instruction -> Instruction
flipInstruction On = Off
flipInstruction Off = On

intParser :: Parser Int
intParser =
  let numParser = read <$> P.many1 P.digit
      negativeParser = do
        P.char '-'
        negate <$> numParser
   in P.choice [negativeParser, numParser]

instructionParser :: Parser (Instruction, Cube)
instructionParser = do
  let cubeParser = do
        P.string "x="
        minX <- intParser
        P.string ".."
        maxX <- intParser
        P.string ",y="
        minY <- intParser
        P.string ".."
        maxY <- intParser
        P.string ",z="
        minZ <- intParser
        P.string ".."
        maxZ <- intParser
        pure $ Cube (minX, maxX) (minY, maxY) (minZ, maxZ)
      onParser = do
        P.string "on "
        (On,) <$> cubeParser
      offParser = do
        P.string "off "
        (Off,) <$> cubeParser
   in P.choice [P.try onParser, offParser]

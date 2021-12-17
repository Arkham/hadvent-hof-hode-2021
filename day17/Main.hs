{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse targetParser "" contents of
    Right target@(Target (minX, maxX) (minY, maxY)) -> do
      let options = [(velX, velY) | velX <- [1 .. maxX], velY <- [minY .. maxX]]
      let results =
            mapMaybe
              ( \vel ->
                  simulate (State (0, 0) vel 0) target
              )
              options
      print $ maximum $ map (highestY . snd) results
      print $ length results
    Left _ ->
      putStrLn "could not parse input"

simulate :: State -> Target -> Maybe ((Int, Int), State)
simulate state target
  | within target newPos = Just (newPos, newState)
  | goneFurther target newPos = Nothing
  | otherwise = simulate newState target
  where
    newState@(State newPos _ _) = tick state

data State = State
  { position :: (Int, Int),
    velocity :: (Int, Int),
    highestY :: Int
  }
  deriving (Show)

tick :: State -> State
tick (State (x, y) (velX, velY) highestY) =
  let newX = x + velX
      newY = y + velY
      newVelX
        | velX > 0 = velX - 1
        | velX == 0 = 0
        | velX < 0 = velX + 1
      newHighestY =
        if newY > highestY
          then newY
          else highestY
   in State (x + velX, y + velY) (newVelX, velY - 1) newHighestY

data Target
  = Target (Int, Int) (Int, Int)
  deriving (Show)

within :: Target -> (Int, Int) -> Bool
within (Target (minX, maxX) (minY, maxY)) (x, y) =
  x >= minX
    && x <= maxX
    && y >= minY
    && y <= maxY

goneFurther :: Target -> (Int, Int) -> Bool
goneFurther (Target (_, maxX) (minY, _)) (x, y) =
  x > maxX || y < minY

targetParser :: Parser Target
targetParser = do
  P.string "target area: x="
  xMin <- intParser
  P.string ".."
  xMax <- intParser
  P.string ", y="
  yMin <- intParser
  P.string ".."
  yMax <- intParser
  pure $ Target (xMin, xMax) (yMin, yMax)

intParser :: Parser Int
intParser =
  let numParser = read <$> P.many1 P.digit
      negativeParser = do
        P.char '-'
        negate <$> numParser
   in P.choice [negativeParser, numParser]

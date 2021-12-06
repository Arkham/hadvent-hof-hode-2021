{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let timers = map toInt $ T.splitOn "," contents
  let initial =
        foldl
          ( flip $
              HM.alter
                ( \case
                    Just v -> Just (v + 1)
                    Nothing -> Just 1
                )
          )
          HM.empty
          timers
  print $ sum $ HM.elems $ foldl evolve initial [1 .. 80]
  print $ sum $ HM.elems $ foldl evolve initial [1 .. 256]

toInt :: T.Text -> Int
toInt =
  read . T.unpack

evolve :: HM.HashMap Int Int -> Int -> HM.HashMap Int Int
evolve acc _ =
  let howManyZeroes = HM.findWithDefault 0 0 acc
      rest = List.filter (\(val, _) -> val /= 0) (HM.toList acc)
      updatedRest = map (\(val, count) -> (val - 1, count)) rest
   in case howManyZeroes of
        0 ->
          HM.fromList updatedRest
        _ ->
          HM.alter
            ( \case
                Just v -> Just $ v + howManyZeroes
                Nothing -> Just howManyZeroes
            )
            6
            (HM.fromList $ (8, howManyZeroes) : updatedRest)

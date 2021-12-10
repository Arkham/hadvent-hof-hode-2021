{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let lines = map T.unpack (T.lines contents)
  let results = map analyzeLine lines
  print $
    sum $
      mapMaybe
        ( \case
            Incomplete _ ->
              Nothing
            Corrupted char ->
              HM.lookup char illegalValues
        )
        results
  let sortedScores =
        List.sort $
          mapMaybe
            ( \case
                Incomplete stack ->
                  Just $
                    foldl
                      ( \acc el ->
                          acc * 5
                            + HM.lookupDefault 0 el stackValues
                      )
                      0
                      (mapMaybe (`HM.lookup` startToEnd) stack)
                Corrupted char ->
                  Nothing
            )
            results
  print $ sortedScores !! (length sortedScores `div` 2)

data AnalysisResult
  = Incomplete String
  | Corrupted Char

analyzeLine :: String -> AnalysisResult
analyzeLine input =
  go input []
  where
    go str stack =
      case str of
        [] ->
          Incomplete stack
        first : rest ->
          case ( HM.lookup first startToEnd,
                 HM.lookup first endToStart,
                 stack
               ) of
            (Just _, _, _) ->
              go rest (first : stack)
            (_, Just start, stackFirst : stackRest) ->
              if start == stackFirst
                then go rest stackRest
                else Corrupted first
            _ ->
              Incomplete stack

startToEnd :: HM.HashMap Char Char
startToEnd =
  HM.fromList
    [ ('(', ')'),
      ('[', ']'),
      ('{', '}'),
      ('<', '>')
    ]

endToStart :: HM.HashMap Char Char
endToStart =
  HM.fromList
    [ (')', '('),
      (']', '['),
      ('}', '{'),
      ('>', '<')
    ]

illegalValues :: HM.HashMap Char Int
illegalValues =
  HM.fromList
    [ (')', 3),
      (']', 57),
      ('}', 1197),
      ('>', 25137)
    ]

stackValues :: HM.HashMap Char Int
stackValues =
  HM.fromList
    [ (')', 1),
      (']', 2),
      ('}', 3),
      ('>', 4)
    ]

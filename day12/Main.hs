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
  let connections = mapMaybe parseLine (T.lines contents)
  let caveMap =
        foldl
          ( \acc (start, end) ->
              updateMap start end $
                updateMap end start acc
          )
          HM.empty
          connections
  print $
    length $
      findPaths
        caveMap
        ( \next path ->
            isLower next && elem next path
        )
  print $
    length $
      findPaths
        caveMap
        ( \next path ->
            let currentMax =
                  maximum $
                    map length $
                      List.group $
                        List.sort $
                          filter isLower path
             in isLower next && currentMax > 1 && elem next path
        )

type Cave = T.Text

updateMap ::
  Cave ->
  Cave ->
  HM.HashMap Cave [Cave] ->
  HM.HashMap Cave [Cave]
updateMap start end =
  HM.alter
    ( \case
        Just v ->
          Just (end : v)
        Nothing ->
          Just [end]
    )
    start

parseLine :: T.Text -> Maybe (Cave, Cave)
parseLine input =
  case T.splitOn "-" input of
    [start, end] ->
      Just (start, end)
    _ ->
      Nothing

findPaths ::
  HM.HashMap Cave [Cave] ->
  (Cave -> [Cave] -> Bool) ->
  [[Cave]]
findPaths caveMap isInvalid =
  go [["start"]] []
  where
    go [] results = map reverse results
    go paths results =
      let (newPaths, newResults) =
            unzip $
              mapMaybe
                ( \path ->
                    fmap
                      ( \values ->
                          let (newPaths, newResults) = unzip $ List.map (step path) values
                           in (concat newPaths, concat newResults)
                      )
                      (HM.lookup (head path) caveMap)
                )
                paths
       in go (concat newPaths) (concat newResults <> results)

    step path next
      | next == "end" =
        ([], [next : path])
      | next == "start" || isInvalid next path =
        ([], [])
      | otherwise =
        ([next : path], [])

isLower :: T.Text -> Bool
isLower input = T.toLower input == input

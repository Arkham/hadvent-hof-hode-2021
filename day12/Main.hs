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
updateMap _ "start" acc = acc
updateMap "end" _ acc = acc
updateMap start end acc =
  HM.alter
    ( \case
        Just v ->
          Just (end : v)
        Nothing ->
          Just [end]
    )
    start
    acc

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
            foldl
              ( \acc path ->
                  let nextHops = HM.lookupDefault [] (head path) caveMap
                   in foldl (step path) acc nextHops
              )
              ([], results)
              paths
       in go newPaths newResults

    step path (pathsAcc, resultAcc) next
      | next == "end" =
        (pathsAcc, (next : path) : resultAcc)
      | isInvalid next path =
        (pathsAcc, resultAcc)
      | otherwise =
        ((next : path) : pathsAcc, resultAcc)

isLower :: T.Text -> Bool
isLower input = T.toLower input == input

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Numeric (readHex)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let bits =
        List.concatMap toBinary $
          mapMaybe parseHex (T.unpack contents)
  let result = fst $ parseBits bits
  print $ sumVersions result
  print $ evaluateExpr result

data Packet
  = Literal
      { version :: Int,
        value :: Int
      }
  | Operator
      { version :: Int,
        operator :: Int,
        operands :: [Packet]
      }
  deriving (Show)

sumVersions :: Packet -> Int
sumVersions packet =
  case packet of
    Literal {version} ->
      version
    Operator {version, operands} ->
      version + sum (map sumVersions operands)

evaluateExpr :: Packet -> Int
evaluateExpr packet =
  case packet of
    Literal {value} ->
      value
    Operator {operator, operands} ->
      case (operator, operands) of
        (0, _) ->
          sum $ map evaluateExpr operands
        (1, _) ->
          product $ map evaluateExpr operands
        (2, _) ->
          minimum $ map evaluateExpr operands
        (3, _) ->
          maximum $ map evaluateExpr operands
        (5, [first, second]) ->
          if evaluateExpr first > evaluateExpr second
            then 1
            else 0
        (6, [first, second]) ->
          if evaluateExpr first < evaluateExpr second
            then 1
            else 0
        (7, [first, second]) ->
          if evaluateExpr first == evaluateExpr second
            then 1
            else 0

parseBits :: [Int] -> (Packet, [Int])
parseBits bits =
  let (version, afterVersion) = List.splitAt 3 bits
   in case List.splitAt 3 afterVersion of
        ([1, 0, 0], literalBits) ->
          parseLiteral (toInt version) literalBits
        (operator, operatorBits) ->
          parseOperator (toInt version) (toInt operator) operatorBits

parseLiteral :: Int -> [Int] -> (Packet, [Int])
parseLiteral version bits =
  ( Literal
      { version = version,
        value = toInt value
      },
    leftover
  )
  where
    (value, leftover) = go bits [] []
    go [] acc leftover = (concat $ reverse acc, leftover)
    go bits acc leftover =
      let (chunk, rest) = List.splitAt 5 bits
       in case chunk of
            1 : value ->
              go rest (value : acc) []
            0 : value ->
              go [] (value : acc) rest

parseOperator :: Int -> Int -> [Int] -> (Packet, [Int])
parseOperator version operator bits =
  case bits of
    0 : xs ->
      -- next 15 bits represent the total length in bits
      let (subLength, rest) = List.splitAt 15 xs
          (subRest, leftover) = List.splitAt (toInt subLength) rest

          go [] acc = reverse acc
          go input acc =
            let (v, more) = parseBits input
             in go more (v : acc)
       in ( Operator
              { version = version,
                operator = operator,
                operands = go subRest []
              },
            leftover
          )
    1 : xs ->
      -- next 11 bits represent the number of subpackets
      let (subNum, rest) = List.splitAt 11 xs
          (result, leftover) = go (toInt subNum) rest []

          go 0 leftover acc = (reverse acc, leftover)
          go n input acc =
            let (v, more) = parseBits input
             in go (n - 1) more (v : acc)
       in ( Operator
              { version = version,
                operator = operator,
                operands = result
              },
            leftover
          )

-- parse input
parseHex :: Char -> Maybe Int
parseHex c =
  case readHex [c] of
    (val, _) : _ -> Just val
    _ -> Nothing

toBinary :: Int -> [Int]
toBinary input =
  leftPad $ reverse $ go input
  where
    go 0 = []
    go n =
      let (result, remainder) = n `divMod` 2
       in remainder : go result

    leftPad xs =
      replicate (4 - length xs) 0 ++ xs

toInt :: [Int] -> Int
toInt = foldl (\acc e -> acc * 2 + e) 0

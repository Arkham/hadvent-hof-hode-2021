{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM_)
import Data.Either (rights)
import Data.Foldable (Foldable)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let numbers = rights $ map (P.parse numberParser "") (T.lines contents)
  print $
    magnitude $
      foldl1
        ( \acc el ->
            reduce $ Pair acc el
        )
        numbers
  print $
    maximum $
      [ (magnitude . reduce . uncurry Pair) (x, y)
        | x <- numbers,
          y <- numbers,
          x /= y
      ]

data Number
  = Regular Int
  | Pair Number Number
  deriving (Eq)

magnitude :: Number -> Int
magnitude (Regular v) = v
magnitude (Pair left right) =
  magnitude left * 3 + magnitude right * 2

data ExplodeResult
  = NoExplosion
  | Donate Int Int Number
  | DonateLeft Int Number
  | DonateRight Int Number
  | Exploded Number
  deriving (Show)

reduce :: Number -> Number
reduce num =
  case explode 0 num of
    DonateLeft _ new -> reduce new
    DonateRight _ new -> reduce new
    Exploded new -> reduce new
    NoExplosion ->
      case split num of
        Split new ->
          reduce new
        NoSplit ->
          num

data SplitResult
  = Split Number
  | NoSplit

split :: Number -> SplitResult
split (Regular n)
  | n >= 10 =
    let smaller = div n 2
     in Split $ Pair (Regular smaller) (Regular (n - smaller))
  | otherwise = NoSplit
split (Pair l r) =
  case split l of
    Split newL ->
      Split $ Pair newL r
    NoSplit ->
      case split r of
        Split newR ->
          Split $ Pair l newR
        NoSplit ->
          NoSplit

explode :: Int -> Number -> ExplodeResult
explode depth (Regular v) = NoExplosion
explode 4 (Pair (Regular l) (Regular r)) = Donate l r $ Regular 0
explode depth (Pair left right) =
  case explode (depth + 1) left of
    Donate dl dr newL ->
      DonateLeft dl $ Pair newL (addToLeft dr right)
    DonateLeft v newL ->
      DonateLeft v $ Pair newL right
    DonateRight v newL ->
      Exploded $ Pair newL (addToLeft v right)
    Exploded newL ->
      Exploded $ Pair newL right
    NoExplosion ->
      case explode (depth + 1) right of
        Donate dl dr newR ->
          DonateRight dr $ Pair (addToRight dl left) newR
        DonateLeft v newR ->
          Exploded $ Pair (addToRight v left) newR
        DonateRight v newR ->
          DonateRight v $ Pair left newR
        Exploded newR ->
          Exploded $ Pair left newR
        NoExplosion ->
          NoExplosion

addToLeft amount (Regular v) = Regular $ v + amount
addToLeft amount (Pair l r) = Pair (addToLeft amount l) r

addToRight amount (Regular v) = Regular $ v + amount
addToRight amount (Pair l r) = Pair l (addToRight amount r)

instance Show Number where
  show (Regular v) = show v
  show (Pair left right) = "[" <> show left <> "," <> show right <> "]"

numberParser :: Parser Number
numberParser =
  let intParser = Regular . read <$> P.many1 P.digit
      pairParser = do
        P.char '['
        left <- numberParser
        P.char ','
        right <- numberParser
        P.char ']'
        pure $ Pair left right
   in P.choice [intParser, pairParser]

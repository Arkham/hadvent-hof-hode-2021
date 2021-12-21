module Main where

import qualified Control.Monad as Mon
import qualified Control.Monad.State as MonSt
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let (initial1, initial2) = (7, 4)
  let game = ((initial1, 0), (initial2, 0))
  part1 game
  part2 game

data Player
  = P1
  | P2

type Pair = (Int, Int)

type Game = (Pair, Pair)

part1 :: Game -> IO ()
part1 (initial1, initial2) = do
  let playOrder = cycle [P1, P2]
  let diceRolls = cycle [1 .. 100]
  let ((_, s1), (_, s2), _, count) =
        head $
          dropWhile (\((_, s1), (_, s2), _, _) -> s1 < 1000 && s2 < 1000) $
            scanl
              ( \(p1@(p1pos, p1score), p2@(p2pos, p2score), rolls, count) player ->
                  let (rolled, restRolls) = splitAt 3 rolls
                   in case player of
                        P1 ->
                          (move p1 (sum rolled), p2, restRolls, count + 3)
                        P2 ->
                          (p1, move p2 (sum rolled), restRolls, count + 3)
              )
              (initial1, initial2, diceRolls, 0)
              playOrder
  print $ count * min s1 s2

move :: Pair -> Int -> Pair
move (pos, score) value =
  let moddedAdd start sum =
        mod (start + sum - 1) 10 + 1
      newPos = moddedAdd pos value
   in (newPos, score + newPos)

-- part 2

runFibonacci :: IO ()
runFibonacci =
  print $ MonSt.evalState (fibState 150) M.empty

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibState :: Int -> MonSt.State (M.Map Int Int) Int
fibState n
  | n == 0 = return 1
  | n == 1 = return 1
  | otherwise = do
    cache <- MonSt.get
    case M.lookup n cache of
      Just v -> return v
      Nothing -> do
        minusTwo <- fibState (n - 2)
        minusOne <- fibState (n - 1)
        let result = minusOne + minusTwo
        MonSt.modify (M.insert n result)
        return result

solveState :: Game -> MonSt.State (M.Map Game (Int, Int)) (Int, Int)
solveState game@(p1@(_, s1), p2@(_, s2))
  | s1 >= 21 = return (1, 0)
  | s2 >= 21 = return (0, 1)
  | otherwise = do
    cache <- MonSt.get
    case M.lookup game cache of
      Just v -> return v
      Nothing -> do
        (p2wins, p1wins) <-
          foldl (\(x, y) (a, b) -> (x + a, y + b)) (0, 0)
            <$> traverse solveState [(p2, move p1 step) | step <- quantumRolls]
        MonSt.modify (M.insert game (p1wins, p2wins))
        return (p1wins, p2wins)
  where
    quantumRolls = sum <$> Mon.replicateM 3 [1, 2, 3]

part2 :: Game -> IO ()
part2 game = do
  let (x, y) = MonSt.evalState (solveState game) M.empty
  print (max x y)

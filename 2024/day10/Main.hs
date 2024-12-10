module Main where

import Aoc (runAll)
import Data.Char (digitToInt)
import Data.HashSet (HashSet, empty, singleton, size, unions)
import Data.Matrix (Matrix, getElem, ncols, nrows)
import qualified Data.Matrix as Mat

nextSteps :: Matrix Int -> (Int, Int) -> [(Int, Int)]
nextSteps m p@(x, y) =
  filter ((== currHeight + 1) . height m) . filter inside
    $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    inside (x', y') = x' > 0 && y' > 0 && x' <= ncols m && y' <= nrows m
    currHeight = height m p

height :: Matrix Int -> (Int, Int) -> Int
height m (x', y') = getElem x' y' m

reachablePositions :: Matrix Int -> (Int, Int) -> HashSet (Int, Int)
reachablePositions m p
  | null nextSteps' =
    if height m p == 9
      then singleton p
      else empty
  | otherwise = unions $ map (reachablePositions m) nextSteps'
  where
    nextSteps' = nextSteps m p

reachablePaths :: Matrix Int -> (Int, Int) -> Int
reachablePaths m p
  | null nextSteps' =
    if height m p == 9
      then 1
      else 0
  | otherwise = sum $ map (reachablePaths m) nextSteps'
  where
    nextSteps' = nextSteps m p

trailScore :: Matrix Int -> (Int, Int) -> Int
trailScore = (size .) . reachablePositions

trailheads :: Matrix Int -> [(Int, Int)]
trailheads m =
  let ys = [1 .. nrows m]
      xs = [1 .. ncols m]
      combinations = [(x, y) | x <- xs, y <- ys]
   in filter ((== 0) . height m) combinations

solve :: (Matrix Int -> (Int, Int) -> Int) -> String -> Int
solve f s =
  let m = Mat.fromLists . map (map digitToInt) $ lines s
      ts = trailheads m
   in sum $ map (f m) ts

solve1 :: String -> Int
solve1 = solve trailScore

solve2 :: String -> Int
solve2 = solve reachablePaths

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day10/example.txt")
  , (solve1, "day10/input.txt")
  , (solve2, "day10/example.txt")
  , (solve2, "day10/input.txt")
  ]

main :: IO ()
main = runAll tasks

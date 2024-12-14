module Main where

import Aoc (runAll)
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Regex.TDFA

digitsRegex :: String
digitsRegex = "[^0-9]*([0-9]+)[^0-9]*([0-9]+)"

matches :: String -> Maybe (Int, Int)
matches s =
  case (s =~ digitsRegex :: [[String]]) of
    [] -> Nothing
    [[_, x, y]] -> Just (read x, read y)
    _ -> error "invalid input"

cramerSolve :: (Int, Int, Int, Int, Int, Int) -> (Int, Int)
cramerSolve (a1, b1, a2, b2, d1, d2) =
  let det = a1 * b2 - a2 * b1
      detA = d1 * b2 - d2 * b1
      detB = a1 * d2 - a2 * d1
   in (div detA det, div detB det)

parseInput :: String -> [[(Int, Int)]]
parseInput s = chunksOf 3 $ mapMaybe matches $ lines s

calcTokens :: Int -> [(Int, Int)] -> Int
calcTokens offset [(x1, y1), (x2, y2), (x3, y3)] =
  let x3' = x3 + offset
      y3' = y3 + offset
      (aTimes, bTimes) = cramerSolve (x1, x2, y1, y2, x3', y3')
   in if aTimes * x1 + bTimes * x2 == x3' && aTimes * y1 + bTimes * y2 == y3'
        then aTimes * 3 + bTimes
        else 0
calcTokens _ _ = error "invalid input"

solve1 :: String -> Int
solve1 = sum . map (calcTokens 0) . parseInput

solve2 :: String -> Int
solve2 = sum . map (calcTokens 10000000000000) . parseInput

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day13/example.txt")
  , (solve1, "day13/input.txt")
  , (solve2, "day13/example.txt")
  , (solve2, "day13/input.txt")
  ]

main :: IO ()
main = runAll tasks

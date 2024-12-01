module Main where

import Aoc (runAll)
import Data.List (sort)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "Line must have two integers separated by whitespace"

parseLineToPair :: String -> (Int, Int)
parseLineToPair = tuplify . map read . words

pairs :: String -> [(Int, Int)]
pairs = uncurry zip . pairMap sort . unzip . map parseLineToPair . lines

solve1 :: String -> Int
solve1 = sum . map (uncurry ((abs .) . (-))) . pairs

solve2 :: String -> Int
solve2 input =
  let (lhs, rhs) = unzip . map parseLineToPair $ lines input
   in sum $ map (\item -> item * (length . filter (== item) $ rhs)) lhs

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day01/example.txt")
  , (solve1, "day01/input.txt")
  , (solve2, "day01/example.txt")
  , (solve2, "day01/input.txt")
  ]

main :: IO ()
main = runAll tasks

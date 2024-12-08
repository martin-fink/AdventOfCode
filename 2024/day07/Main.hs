module Main where

import Aoc (runAll)
import Control.Monad (replicateM)

eval :: Int -> [(Int, Int -> Int -> Int)] -> Int
eval n ((v, f):xs) = eval (f n v) xs
eval n [] = n

concatNum :: Int -> Int -> Int
concatNum x y = read $ show x ++ show y

checkCombinations :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
checkCombinations operators expected (x:xs) =
  let n = length xs
      combinations = replicateM n operators
   in any ((\nums -> expected == eval x nums) . zip xs) combinations

stripSuffix :: String -> String
stripSuffix ":" = ""
stripSuffix [] = []
stripSuffix (c:s) = c : stripSuffix s

filterLine :: [Int -> Int -> Int] -> String -> Int
filterLine operators s =
  let (expected:nums) = words s
      expected' = read $ stripSuffix expected
      nums' = map read nums
   in if checkCombinations operators expected' nums'
        then expected'
        else 0

solve :: [Int -> Int -> Int] -> String -> Int
solve operators = sum . map (filterLine operators) . lines

solve1 :: String -> Int
solve1 = solve [(*), (+)]

solve2 :: String -> Int
solve2 = solve [(*), (+), concatNum]

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day07/example.txt")
  , (solve1, "day07/input.txt")
  , (solve2, "day07/example.txt")
  , (solve2, "day07/input.txt")
  ]

main :: IO ()
main = runAll tasks
-- [1, 2, 3]
--

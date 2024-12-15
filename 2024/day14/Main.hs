module Main where

import Aoc (runAll)
import qualified Data.HashSet as Set
import Debug.Trace
import Text.Regex.TDFA

digitsRegex :: String
digitsRegex = "p=(.+),(.+) v=(.+),(.+)"

matches :: String -> (Int, Int, Int, Int)
matches s =
  case (s =~ digitsRegex :: [[String]]) of
    [[_, x, y, vx, vy]] -> (read x, read y, read vx, read vy)
    x -> error ("invalid input" ++ show x ++ s)

step :: Int -> Int -> Int -> (Int, Int, Int, Int) -> (Int, Int)
step times w h (x, y, vx, vy) = (mod (x + times * vx) w, mod (y + times * vy) h)

safetyRating :: Int -> Int -> [(Int, Int)] -> Int
safetyRating w h pos =
  let tl = filter (\(x, y) -> x < div w 2 && y < div h 2)
      tr = filter (\(x, y) -> x > div w 2 && y < div h 2)
      bl = filter (\(x, y) -> x < div w 2 && y > div h 2)
      gr = filter (\(x, y) -> x > div w 2 && y > div h 2)
   in foldr ((*) . length . ($ pos)) 1 [tl, tr, bl, gr]

-- printPos :: Int -> Int -> [(Int, Int)] -> String
-- printPos w h pos = unlines [[if (x, y) `elem` pos then '#' else '.' | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
printPos :: Int -> Int -> [(Int, Int)] -> String
printPos width height positions =
  let positionSet = Set.fromList positions
   in unlines
        [ [ if Set.member (x, y) positionSet
          then '#'
          else '.'
        | x <- [0 .. width - 1]
        ]
        | y <- [0 .. height - 1]
        ]

longestContiguousHashes :: String -> Int
longestContiguousHashes = maximum . map (length . filter (== '#')) . lines

solve1 :: Int -> Int -> String -> Int
solve1 w h s =
  let inputs = map matches $ lines s
      positions = map (step 100 w h) inputs
   in safetyRating w h positions

solve2 :: String -> Int
solve2 s =
  let w = 101
      h = 103
      -- calculated using chinese remainder theorem:
      -- first "pattern" shows up at step 14 vertically and 64 horizontally
      -- then we need to solve the crt for 14 + n * 101 and 64 + n * 103
      solution = 7892
      inputs = map matches $ lines s
      easterEgg = printPos w h $ map (step solution w h) inputs
   in trace easterEgg solution

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1 11 7, "day14/example.txt")
  , (solve1 101 103, "day14/input.txt")
  , (solve2, "day14/input.txt")
  ]

main :: IO ()
main = runAll tasks

module Main where

import Aoc (runAll)

prependMaybe :: Maybe a -> [a] -> [a]
prependMaybe Nothing xs = xs
prependMaybe (Just x) xs = x : xs

isSafe :: Bool -> Maybe Int -> (Int -> Int -> Bool) -> [Int] -> Bool
isSafe True prev cmp (x : x' : xs)
  | not $ itemsSafe cmp x x' =
      let safeFn = isSafe False
       in safeFn Nothing cmp (x : xs)
            || safeFn (Just x) cmp (prependMaybe prev (x' : xs))
isSafe c _ cmp (x : x' : xs) = itemsSafe cmp x x' && isSafe c (Just x) cmp (x' : xs)
isSafe _ _ _ _ = True

itemsSafe :: (Int -> Int -> Bool) -> Int -> Int -> Bool
itemsSafe cmp x y =
  let diff = abs (x - y)
   in diff >= 1 && diff <= 3 && cmp x y

solve :: Bool -> String -> Int
solve skipOne s =
  let nums = map (map read . words) . lines $ s
      safeFn = isSafe skipOne Nothing
   in length . filter (\x -> safeFn (<) x || safeFn (>) x) $ nums

solve1 :: String -> Int
solve1 = solve False

solve2 :: String -> Int
solve2 = solve True

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day02/example.txt"),
    (solve1, "day02/input.txt"),
    (solve2, "day02/example.txt"),
    (solve2, "day02/input.txt")
  ]

main :: IO ()
main = runAll tasks

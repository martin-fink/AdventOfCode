module Main where

import Aoc (runAll)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (mapMaybe)
import Debug.Trace

data Tower =
  Tower Char Int Int

towerTy :: Tower -> Char
towerTy (Tower c _ _) = c

toTower :: Int -> Int -> Char -> Maybe Tower
toTower i j c
  | isDigit c || isAsciiLower c || isAsciiUpper c = Just $ Tower c i j
  | otherwise = Nothing

buildMatrix :: String -> Matrix Char
buildMatrix = Matrix.fromLists . lines

findTowers :: String -> [Tower]
findTowers =
  concatMap (\(i, line) -> mapMaybe (uncurry $ toTower i) $ zip [0 ..] line)
    . zip [0 ..]
    . lines

towerCombinations :: [Tower] -> [(Tower, Tower)]
towerCombinations =
  concatMap combinations
    . groupBy (\a b -> towerTy a == towerTy b)
    . sortBy (\a b -> compare (towerTy a) (towerTy b))

combinations :: [Tower] -> [(Tower, Tower)]
combinations = map toTuple . filter ((2 ==) . length) . subsequences
  where
    toTuple [a, b] = (a, b)
    toTuple _ = error "is not a tuple"

isInside :: Int -> Int -> (Int, Int) -> Bool
isInside w h (x, y) = x >= 0 && y >= 0 && x < w && y < h

solve :: (Int -> Int -> Tower -> Tower -> [(Int, Int)]) -> String -> Int
solve f s =
  let tuples = towerCombinations $ findTowers s
      w = length $ head (lines s)
      h = length $ lines s
      locations = map head . group . sort $ concatMap (uncurry $ f w h) tuples
   in length locations

solve1 :: String -> Int
solve1 = solve simpleAntinodes

solve2 :: String -> Int
solve2 = solve resonantAntinodes

simpleAntinodes :: Int -> Int -> Tower -> Tower -> [(Int, Int)]
simpleAntinodes w h (Tower _ x1 y1) (Tower _ x2 y2) =
  let xDiff = x2 - x1
      yDiff = y2 - y1
      inside = isInside w h
   in filter inside [(x1 - xDiff, y1 - yDiff), (x2 + xDiff, y2 + yDiff)]

resonantAntinodes :: Int -> Int -> Tower -> Tower -> [(Int, Int)]
resonantAntinodes w h (Tower _ x1 y1) (Tower _ x2 y2) =
  let xDiff = x2 - x1
      yDiff = y2 - y1
      inside = isInside w h
      fn f x' y' =
        takeWhile inside
          $ map (\n -> (f x' (xDiff * n), f y' (yDiff * n))) [0 ..]
   in fn (-) x1 y1 ++ fn (+) x2 y2

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day08/example.txt")
  , (solve1, "day08/input.txt")
  , (solve2, "day08/ex2.txt")
  , (solve2, "day08/example.txt")
  , (solve2, "day08/input.txt")
  ]

main :: IO ()
main = runAll tasks

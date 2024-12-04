{-# LANGUAGE ViewPatterns #-}

module Main where

import Aoc (runAll)
import Data.List (sort, stripPrefix, transpose)
import Data.Universe.Helpers (diagonals)

countXmas :: String -> Int
countXmas (stripPrefix "XMAS" -> Just xs) = 1 + countXmas ("MAS" ++ xs)
countXmas (stripPrefix "SAMX" -> Just xs) = 1 + countXmas ("AMX" ++ xs)
countXmas (_:xs) = countXmas xs
countXmas [] = 0

windows :: [a] -> [[a]]
windows (x:y:z:xs) = [x, y, z] : windows (y : z : xs)
windows _ = []

allLines :: String -> [String]
allLines s =
  let matrix = lines s
   in matrix
        ++ transpose matrix
        ++ diagonals matrix
        ++ diagonals ((reverse . transpose) matrix)

xWindows :: [String] -> [(Char, Char, Char, Char, Char)]
xWindows = map toTuple . concatMap windows . transpose . map windows
  where
    toTuple [[xa, _, xc], [_, yb, _], [za, _, zc]] = (yb, xa, zc, xc, za)
    toTuple x = error $ show x

isMasCross :: (Char, Char, Char, Char, Char) -> Bool
isMasCross ('A', xa, zc, xc, za) = all (("MS" ==) . sort) [[xa, zc], [xc, za]]
isMasCross _ = False

solve1 :: String -> Int
solve1 = sum . map countXmas . allLines

solve2 :: String -> Int
solve2 = length . filter isMasCross . xWindows . lines

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day04/example.txt")
  , (solve1, "day04/input.txt")
  , (solve2, "day04/example.txt")
  , (solve2, "day04/input.txt")
  ]

main :: IO ()
main = runAll tasks

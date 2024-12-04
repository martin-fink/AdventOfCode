{-# LANGUAGE ViewPatterns #-}

module Main where

import Aoc (runAll)
import Data.List (sort, stripPrefix, transpose)
import Data.Maybe (mapMaybe)
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

xWindows :: [String] -> [(Char, Char, Char, Char)]
xWindows = concatMap (mapMaybe toTuple . windows) . transpose . map windows
  where
    toTuple [[xa, _, xc], [_, 'A', _], [za, _, zc]] = Just (xa, zc, xc, za)
    toTuple _ = Nothing

isMasCross :: (Char, Char, Char, Char) -> Bool
isMasCross (xa, zc, xc, za) = all (("MS" ==) . sort) [[xa, zc], [xc, za]]

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

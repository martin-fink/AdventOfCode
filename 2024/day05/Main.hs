module Main where

import Aoc (runAll)
import Data.Graph
import Data.HashMap.Strict (HashMap, empty, insertWith, lookup)
import Data.HashSet (HashSet, delete, fromList, member, union)
import Data.List.Split (splitOn)

tuplify :: [a] -> (a, a)
tuplify [a, b] = (a, b)
tuplify _ = error "wrong input"

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput = splitParts . tuplify . splitOn "\n\n"

splitParts :: (String, String) -> ([(Int, Int)], [[Int]])
splitParts (lhs, rhs) = (parseLhs lhs, parseRhs rhs)

parseLhs :: String -> [(Int, Int)]
parseLhs = map (tuplify . map read . splitOn "|") . lines

parseRhs :: String -> [[Int]]
parseRhs = map (map read . splitOn ",") . lines

isOrdered :: HashSet Int -> HashMap Int [Int] -> [Int] -> Bool
isOrdered _ _ [] = True
isOrdered illegal l (x:xs)
  | member x illegal = False
  | otherwise =
    let newIllegal =
          union illegal (fromList $ concat (Data.HashMap.Strict.lookup x l))
     in isOrdered newIllegal l xs

toList :: Maybe [a] -> [a]
toList Nothing = []
toList (Just x) = x

findMid :: [a] -> a
findMid xs = xs !! div (length xs) 2

buildHashMap :: [(Int, Int)] -> HashMap Int [Int]
buildHashMap = foldr insertPair empty
  where
    insertPair (v, k) = Data.HashMap.Strict.insertWith (++) k [v]

sortLine :: HashMap Int [Int] -> [Int] -> [Int]
sortLine order = concatMap flattenSCC . stronglyConnComp . map triple
  where
    triple item = (item, item, concat $ Data.HashMap.Strict.lookup item order)

solve1 :: String -> Int
solve1 s =
  let (ord, input) = parseInput s
      l = buildHashMap ord
   in sum . map findMid . filter (isOrdered (fromList []) l) $ input

solve2 :: String -> Int
solve2 s =
  let (ord, input) = parseInput s
      l = buildHashMap ord
   in sum
        . map (findMid . sortLine l)
        . filter (not . isOrdered (fromList []) l)
        $ input

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day05/example.txt")
  , (solve1, "day05/input.txt")
  , (solve2, "day05/example.txt")
  , (solve2, "day05/input.txt")
  ]

main :: IO ()
main = runAll tasks

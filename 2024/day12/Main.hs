module Main where

import Aoc (runAll)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows)

toMatrix :: String -> Matrix Char
toMatrix = fromLists . lines

isOther :: Matrix Char -> (Int, Int) -> (Int, Int) -> Bool
isOther m (x, y) (x', y') =
  let c = getElem y x m
   in x' <= 0 || y' <= 0 || x' > ncols m || y' > nrows m || getElem y' x' m /= c

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

allNeighbours :: (Int, Int) -> [(Int, Int)]
allNeighbours (x, y) =
  [ (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  , (x - 1, y)
  ]

data CornerTy
  = TL
  | TR
  | BL
  | BR

-- Corner type and whether the corner is inverted
type Corner = (CornerTy, Bool)

isCorner :: Matrix Char -> (Int, Int) -> Corner -> Bool
isCorner m pos corner =
  case (corner, map (isOther m pos) $ allNeighbours pos) of
    ((TL, True), [_, True, _, _, _, _, _, True]) -> True
    ((TR, True), [_, True, _, True, _, _, _, _]) -> True
    ((BR, True), [_, _, _, True, _, True, _, _]) -> True
    ((BL, True), [_, _, _, _, _, True, _, True]) -> True
    ((TL, False), [True, False, _, _, _, _, _, False]) -> True
    ((TR, False), [_, False, True, False, _, _, _, _]) -> True
    ((BR, False), [_, _, _, False, True, False, _, _]) -> True
    ((BL, False), [_, _, _, _, _, False, True, False]) -> True
    _ -> False

countCorners :: Matrix Char -> (Int, Int) -> Int
countCorners m pos =
  length
    $ filter
        (isCorner m pos)
        [(c, i) | c <- [TL, TR, BL, BR], i <- [True, False]]

dfsSearch ::
     Matrix Char
  -> HashSet (Int, Int)
  -> (Int, Int)
  -> (Int, Int, HashSet (Int, Int))
dfsSearch m seen pos
  | Set.member pos seen = (0, 0, seen)
  | otherwise =
    let next = filter (not . isOther m pos) $ neighbours pos
        fences = length . filter (isOther m pos) $ neighbours pos
        seen' = Set.insert pos seen
     in foldr
          (\(x', y') (a, p, s) -> add (a, p) $ dfsSearch m s (x', y'))
          (1, fences, seen')
          next
  where
    add (x, y) (x', y', s') = (x + x', y + y', s')

perim m pos = length . filter (isOther m pos) $ neighbours pos

dfsSearch' ::
     Matrix Char
  -> HashSet (Int, Int)
  -> (Matrix Char -> (Int, Int) -> Int)
  -> (Int, Int)
  -> (Int, Int, HashSet (Int, Int))
dfsSearch' m seen f pos
  | Set.member pos seen = (0, 0, seen)
  | otherwise =
    let next = filter (not . isOther m pos) $ neighbours pos
        fences = f m pos -- length . filter (f m pos) $ neighbours pos
        seen' = Set.insert pos seen
     in foldr
          (\(x', y') (a, p, s) -> add (a, p) $ dfsSearch' m s f (x', y'))
          (1, fences, seen')
          next
  where
    add (x, y) (x', y', s') = (x + x', y + y', s')

cost :: (Matrix Char -> (Int, Int) -> Int) -> Matrix Char -> Int
cost f m =
  let ys = [1 .. nrows m]
      xs = [1 .. ncols m]
      combinations = [(x, y) | x <- xs, y <- ys]
      seen = Set.empty
   in snd
        $ foldr
            (\pos (s, sum) ->
               let (area, perim, s') = dfsSearch' m s f pos
                in (s', sum + area * perim))
            (seen, 0)
            combinations

solve1 :: String -> Int
solve1 = cost perim . toMatrix

solve2 :: String -> Int
solve2 = cost countCorners . toMatrix

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day12/example.txt")
  , (solve1, "day12/example2.txt")
  , (solve1, "day12/input.txt")
  , (solve2, "day12/example.txt")
  , (solve2, "day12/e.txt")
  , (solve2, "day12/ex3.txt")
  , (solve2, "day12/input.txt")
  ]

main :: IO ()
main = runAll tasks

{-# LANGUAGE DeriveGeneric #-}

module Main where

import Aoc (runAll)
import Data.HashSet (HashSet, insert, member)
import qualified Data.HashSet as HashSet (empty, fromList, toList)
import Data.Hashable
import Data.List (elemIndex, findIndex)
import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows)
import qualified Data.Matrix as Matrix
import GHC.Generics (Generic)

data Pos =
  Pos Int Int Dir
  deriving (Eq, Generic)

instance Hashable Pos

data Dir
  = U
  | D
  | L
  | R
  deriving (Eq, Generic)

instance Hashable Dir

turn :: Dir -> Dir
turn U = R
turn R = D
turn D = L
turn L = U

step :: Pos -> Pos
step (Pos x y U) = Pos (x - 1) y U
step (Pos x y D) = Pos (x + 1) y D
step (Pos x y L) = Pos x (y - 1) L
step (Pos x y R) = Pos x (y + 1) R

isInside :: Int -> Int -> Pos -> Bool
isInside w h (Pos x y _) = x > 0 && y > 0 && x <= w && y <= h

toMatrix :: String -> Matrix Char
toMatrix = fromLists . lines

findStart :: String -> Pos
findStart s =
  let xs = lines s
      r = unwrap $ findIndex (elem '^') xs
      c = unwrap $ elemIndex '^' (xs !! r)
   in Pos (1 + r) (1 + c) U
  where
    unwrap (Just x) = x
    unwrap Nothing = error "nothing"

locations :: Matrix Char -> Pos -> HashSet Pos -> (Bool, HashSet (Int, Int))
locations m p@(Pos x y dir) steps
  | not . inside $ p = (False, removeDir steps)
  | member p steps = (True, removeDir steps)
  | inside (step p)
      && let Pos x' y' _ = step p
          in getElem x' y' m == '#' =
    locations m (Pos x y $ turn dir) (insert p steps)
  | otherwise = locations m (step p) (insert p steps)
  where
    inside = isInside (ncols m) (nrows m)

removeDir :: HashSet Pos -> HashSet (Int, Int)
removeDir = HashSet.fromList . map (\(Pos x' y' _) -> (x', y')) . HashSet.toList

producesCycle :: Matrix Char -> Pos -> Pos -> Bool
producesCycle m p (Pos x y _) =
  let m' = Matrix.setElem '#' (x, y) m
   in fst $ locations m' p HashSet.empty

cycleCreationPoints ::
     Matrix Char -> Pos -> Pos -> HashSet Pos -> HashSet (Int, Int)
cycleCreationPoints m initial p@(Pos x y dir) cyclePoints
  | not . inside $ p = removeDir cyclePoints
  | inside (step p)
      && let Pos x' y' _ = step p
          in getElem x' y' m == '#' =
    cycleCreationPoints m initial (Pos x y $ turn dir) cyclePoints
  | otherwise = cycleCreationPoints m initial (step p) addCyclePoint
  where
    inside = isInside (ncols m) (nrows m)
    addCyclePoint =
      if inside (step p) && producesCycle m initial (step p)
        then insert (step p) cyclePoints
        else cyclePoints

solve1 :: String -> Int
solve1 s =
  let m = toMatrix s
      pos = findStart s
   in length . snd $ locations m pos HashSet.empty

solve2 :: String -> Int
solve2 s =
  let m = toMatrix s
      pos = findStart s
   in length $ cycleCreationPoints m pos pos HashSet.empty

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day06/example.txt")
  , (solve1, "day06/input.txt")
  , (solve2, "day06/example.txt")
  , (solve2, "day06/input.txt")
  ]

main :: IO ()
main = runAll tasks

module Main where

import Aoc (runAll)
import Data.List.Split (splitOn)
import Data.Matrix

parseMat :: (String -> String) -> String -> Matrix Char
parseMat f = fromLists . lines . f

widenMat :: String -> String
widenMat = concatMap widen
  where
    widen '\n' = "\n"
    widen '@' = "@."
    widen 'O' = "[]"
    widen c = [c, c]

findStart :: Matrix Char -> (Int, Int)
findStart m =
  head
    $ filter
        (\(x, y) -> getElem y x m == '@')
        [(x, y) | x <- [1 .. ncols m], y <- [1 .. nrows m]]

nextPos :: Char -> (Int, Int) -> (Int, Int)
nextPos '>' (x, y) = (x + 1, y)
nextPos '<' (x, y) = (x - 1, y)
nextPos '^' (x, y) = (x, y - 1)
nextPos 'v' (x, y) = (x, y + 1)
nextPos _ _ = error "illegal move"

isUpDown :: Char -> Bool
isUpDown '^' = True
isUpDown 'v' = True
isUpDown _ = False

moveWide :: Matrix Char -> (Int, Int) -> Char -> Maybe (Matrix Char)
moveWide m p@(x, y) d =
  let p'@(x', y') = nextPos d p
   in case getElem y x m of
        '.' -> Just m
        '#' -> Nothing
        c ->
          moveWide m p' d >>= \m' ->
            moveWide m' alternatePos d >>= \m'' ->
              let m''' = setElem '.' (y, x) m''
                  m'''' = setElem c (y', x') m'''
               in Just m''''
  where
    alternatePos
      | isUpDown d =
        let p'@(x', y') = nextPos d p
            c = getElem y' x' m
         in case c of
              '[' -> (x' + 1, y')
              ']' -> (x' - 1, y')
              _ -> p'
      | otherwise = nextPos d p

moveRobotWide :: Matrix Char -> (Int, Int) -> Char -> (Matrix Char, (Int, Int))
moveRobotWide m p d =
  case moveWide m p d of
    Nothing -> (m, p)
    Just m' -> (m', nextPos d p)

getMovePos :: Matrix Char -> (Int, Int) -> Char -> Maybe (Int, Int)
getMovePos m p@(x, y) dir =
  case getElem y x m of
    '.' -> Just p
    '#' -> Nothing
    'O' -> getMovePos m (nextPos dir p) dir
    _ -> error "invalid item"

moveRobot :: Matrix Char -> (Int, Int) -> Char -> (Matrix Char, (Int, Int))
moveRobot m p@(x, y) dir =
  case getMovePos m (nextPos dir p) dir of
    Nothing -> (m, p)
    Just (x', y') ->
      let p'@(x'', y'') = nextPos dir p
          c = getElem y'' x'' m
          m' = setElem c (y', x') m
          m'' = setElem '@' (y'', x'') m'
          m''' = setElem '.' (y, x) m''
       in (m''', p')

parseInput :: (String -> String) -> String -> (Matrix Char, [Char])
parseInput f s =
  let [lhs, rhs] = splitOn "\n\n" s
   in (parseMat f lhs, filter (/= '\n') rhs)

calcScore :: Matrix Char -> Int
calcScore m =
  sum
    [ factor (getElem y x m) * (100 * (y - 1) + (x - 1))
    | x <- [1 .. ncols m]
    , y <- [1 .. nrows m]
    ]
  where
    factor 'O' = 1
    factor '[' = 1
    factor _ = 0

solve1 :: String -> Int
solve1 s =
  let (m, moves) = parseInput id s
   in calcScore . fst
        $ foldl (\(m', p) dir -> moveRobot m' p dir) (m, findStart m) moves

solve2 :: String -> Int
solve2 s =
  let (m, moves) = parseInput widenMat s
   in calcScore . fst
        $ foldl (\(m', p) dir -> moveRobotWide m' p dir) (m, findStart m) moves

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day15/example.txt")
  , (solve1, "day15/input.txt")
  , (solve2, "day15/ex2.txt")
  , (solve2, "day15/ex3.txt")
  , (solve2, "day15/input.txt")
  ]

main :: IO ()
main = runAll tasks

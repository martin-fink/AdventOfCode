module Main where

import Aoc (runAll)
import Data.Char (isDigit)

parseNum :: String -> Int -> (Maybe String, String)
parseNum s 0 = (Just "", s)
parseNum (',' : xs) _ = (Just "", ',' : xs)
parseNum (')' : xs) _ = (Just "", ')' : xs)
parseNum (x : xs) n
  | isDigit x =
      case parseNum xs (n - 1) of
        (Just num, s) -> (Just (x : num), s)
        (Nothing, s) -> (Nothing, s)
parseNum (_ : xs) _ = (Nothing, xs)
parseNum [] _ = (Nothing, [])

parseMulImpl :: Bool -> String -> [(Int, Int, Bool)]
parseMulImpl enabled ('m' : 'u' : 'l' : '(' : xs) =
  case parseNum xs 3 of
    (Just num, ',' : xs') ->
      case parseNum xs' 3 of
        (Just num', ')' : xs'') ->
          (read num, read num', enabled) : parseMulImpl enabled xs''
        (_, xs'') -> parseMulImpl enabled xs''
    (_, xs') -> parseMulImpl enabled xs'
parseMulImpl _ ('d' : 'o' : '(' : ')' : xs) = parseMulImpl True xs
parseMulImpl _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = parseMulImpl False xs
parseMulImpl enabled (_ : xs) = parseMulImpl enabled xs
parseMulImpl _ _ = []

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (x, y, _) = (x, y)

tripleLastElem :: (a, b, c) -> c
tripleLastElem (_, _, x) = x

parseMul :: String -> [(Int, Int, Bool)]
parseMul = parseMulImpl True

solve1 :: String -> Int
solve1 = sum . map (uncurry (*) . tripleToPair) . parseMul

solve2 :: String -> Int
solve2 =
  sum
    . map (uncurry (*) . tripleToPair)
    . filter tripleLastElem
    . parseMulImpl True

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day03/example.txt"),
    (solve1, "day03/input.txt"),
    (solve2, "day03/example2.txt"),
    (solve2, "day03/input.txt")
  ]

main :: IO ()
main = runAll tasks

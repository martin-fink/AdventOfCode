module Main where

import Aoc (runAll)

digits :: Int -> Int
digits = length . show

-- tree so we can do log(n) lookups into the memoization
data Tree a =
  Tree (Tree a) a (Tree a)

instance Functor Tree where
  fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n =
  case (n - 1) `divMod` 2 of
    (q, 0) -> index l q
    (q, 1) -> index r q
    _ -> error "unhandled"

nats :: Tree Int
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
      where
        l = n + s
        r = l + s
        s' = s * 2

-- memoization goes brr
-- tree of memoizations
stepList :: Tree (Tree Int)
stepList = fmap (\x -> fmap (step x) nats) nats

step :: Int -> Int -> Int
step 0 _ = 1
step n 0 = index (index stepList (n - 1)) 1
step n d
  | even (digits d) =
    let upper = take (div (digits d) 2) (show d)
        lower = drop (div (digits d) 2) (show d)
     in index (index stepList (n - 1)) (read upper)
          + index (index stepList (n - 1)) (read lower)
  | otherwise = index (index stepList (n - 1)) (d * 2024)

solve1 :: String -> Int
solve1 = sum . map (step 25 . read) . words

solve2 :: String -> Int
solve2 = sum . map (step 75 . read) . words

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day11/example.txt")
  , (solve1, "day11/input.txt")
  , (solve2, "day11/example.txt")
  , (solve2, "day11/input.txt")
  ]

main :: IO ()
main = runAll tasks

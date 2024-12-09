module Main where

import Aoc (runAll)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>))
import qualified Data.Sequence as Seq

data File
  = File Int Int
  | Empty Int

fromInput :: Int -> [Int] -> [File]
fromInput _ [] = []
fromInput fileId (x:x':xs) =
  File fileId x : Empty x' : fromInput (fileId + 1) xs
fromInput fileId [x] = [File fileId x]

parse :: String -> [File]
parse = fromInput 0 . map digitToInt

defragment :: [File] -> [File] -> [File]
defragment _ [] = []
defragment (File fileId' len':worklist) (File fileId len:ys)
  | fileId' == fileId = [File fileId len']
  | otherwise = File fileId len : defragment (File fileId' len' : worklist) ys
defragment (File fileId len:worklist) (Empty emptyLen:ys) =
  let worklist' =
        if len > emptyLen
          then File fileId (len - emptyLen) : worklist
          else worklist
      ys' =
        if emptyLen > len
          then Empty (emptyLen - len) : ys
          else ys
   in File fileId (min len emptyLen) : defragment worklist' ys'
defragment (Empty _:ys) xs = defragment ys xs
defragment [] _ = error "worklist empty"

checksum :: Int -> [File] -> Int
checksum _ [] = 0
checksum pos (Empty len:xs) = checksum (pos + len) xs
checksum pos (File _ 0:xs) = checksum pos xs
checksum pos (File fileId len:xs) =
  pos * fileId + checksum (pos + 1) (File fileId (len - 1) : xs)

printFiles :: [File] -> String
printFiles [] = []
printFiles (Empty len:xs) = replicate len '.' ++ printFiles xs
printFiles (File fileId len:xs) =
  replicate len (intToDigit fileId) ++ printFiles xs

solve1 :: String -> Int
solve1 = checksum 0 . defragment' . parse
  where
    defragment' files = defragment (reverse files) files

solve2 :: String -> Int
solve2 = checksum 0 . toList . defragmentWhole . Seq.fromList . parse

findSlot :: Seq File -> File -> Seq File
findSlot s f@(File _ len) =
  case Seq.viewl s of
    f'@(File _ _) :< rest -> f' <| findSlot rest f
    f'@(Empty len') :< rest ->
      if len' >= len
        then f <| Empty (len' - len) <| rest
        else f' <| findSlot rest f
    _ -> Seq.singleton f
findSlot _ (Empty _) = error "why would we want to move an empty slot"

defragmentWhole :: Seq File -> Seq File
defragmentWhole s =
  case Seq.viewr s of
    rest :> f@(Empty _) -> defragmentWhole rest |> f
    rest :> f@(File fileId len) ->
      case Seq.viewr $ findSlot rest f of
        rest' :> f'@(File fileId' _) ->
          if fileId == fileId'
            then defragmentWhole rest' |> f'
            else defragmentWhole rest' |> f' |> Empty len
        rest' :> Empty len' -> defragmentWhole rest' |> Empty (len' + len)
        _ -> error "we should always find a slot"
    _ -> Seq.empty

tasks :: [(String -> Int, FilePath)]
tasks =
  [ (solve1, "day09/example.txt")
  , (solve1, "day09/input.txt")
  , (solve2, "day09/example.txt")
  , (solve2, "day09/input.txt")
  ]

main :: IO ()
main = runAll tasks

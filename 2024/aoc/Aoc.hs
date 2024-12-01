module Aoc
  ( runAll
  ) where

import System.IO ()

runWithFile :: FilePath -> (String -> a) -> IO a
runWithFile fileName solve = do
  contents <- readFile fileName
  return (solve contents)

runAll :: Show a => [(String -> a, FilePath)] -> IO ()
runAll = mapM_ runTask
  where
    runTask (solve, fileName) = do
      putStrLn $ "file: " ++ fileName
      result <- runWithFile fileName solve
      print result

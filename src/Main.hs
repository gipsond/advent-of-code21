module Main where

import Control.Exception (assert)
import Day1
import Day2

data Part
  = NotStarted
  | Testing (String -> Int)
  | Solved (String -> Int) Int

solveDay :: String -> Part -> Part -> IO ()
solveDay name part1 part2 = do
  input <- readFile $ "data/" ++ name
  putStrLn $ "Day " ++ name
  solvePart input part1
  solvePart input part2

solvePart :: String -> Part -> IO ()
solvePart input NotStarted = return ()
solvePart input (Testing solvePart) = print . solvePart $ input
solvePart input (Solved solvePart expected) =
  let answer = solvePart input
   in assert (expected == answer) (print answer)

main :: IO ()
main = do
  solveDay "1" (Solved Day1.part1 1451)    (Solved Day1.part2 1395)
  solveDay "2" (Solved Day2.part1 1936494) (Solved Day2.part2 1997106066)
module Main where

import Control.Exception (assert)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

data Part
  = NotStarted
  | Testing (String -> Int)
  | Solved  (String -> Int) Int
  | SolvedLong (String -> Int) Int

solveDay :: String -> Part -> Part -> IO ()
solveDay name part1 part2 = do
  input <- readFile $ "data/" ++ name
  putStrLn $ "Day " ++ name
  solvePart input part1
  solvePart input part2

solvePart :: String -> Part -> IO ()
solvePart input NotStarted = return ()
solvePart input (SolvedLong _ _) = putStrLn "Skipping (takes too long)"
solvePart input (Testing solvePart) = print . solvePart $ input
solvePart input (Solved solvePart expected) =
  let answer = solvePart input
   in assert (expected == answer) (print answer)

main :: IO ()
main = do
  solveDay "1" (Solved Day1.part1 1451)    (Solved Day1.part2 1395)
  solveDay "2" (Solved Day2.part1 1936494) (Solved Day2.part2 1997106066)
  solveDay "3" (Solved Day3.part1 3309596) (Solved Day3.part2 2981085)
  solveDay "4" (Solved Day4.part1 25410)   (Solved Day4.part2 2730)
  solveDay "5" (SolvedLong Day5.part1 5197) (SolvedLong Day5.part2 18605)
  solveDay "6" (Solved Day6.part1 386755)  (Solved Day6.part2 1732731810807)
  solveDay "7" (Solved Day7.part1 331067)  (Solved Day7.part2 92881128)
  solveDay "8" (Solved Day8.part1 301)     (Solved Day8.part2 908067)
  
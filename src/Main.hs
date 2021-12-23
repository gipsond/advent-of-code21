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
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19

data Part
  = NotStarted
  | Testing (String -> Int)
  | Solved  (String -> Int) Int
  | SolvedStringAnswer  (String -> String)
  | SolvedLong (String -> Int) Int

solveDay :: String -> Part -> Part -> IO ()
solveDay name part1 part2 = do
  input <- readFile $ "data/" ++ name
  putStrLn $ "Day " ++ name
  solvePart input part1
  solvePart input part2
  putStrLn ""

solvePart :: String -> Part -> IO ()
solvePart input NotStarted = return ()
solvePart input (SolvedLong _ _) = putStrLn "Skipping (takes too long)"
solvePart input (SolvedStringAnswer solvePart) = putStrLn . solvePart $ input
solvePart input (Testing solvePart) = print . solvePart $ input
solvePart input (Solved solvePart expected) =
  let answer = solvePart input
   in assert (expected == answer) (print answer)

main :: IO ()
main = do
  solveDay "1"  (Solved Day1.part1 1451)    (Solved Day1.part2 1395)
  solveDay "2"  (Solved Day2.part1 1936494) (Solved Day2.part2 1997106066)
  solveDay "3"  (Solved Day3.part1 3309596) (Solved Day3.part2 2981085)
  solveDay "4"  (Solved Day4.part1 25410)   (Solved Day4.part2 2730)
  solveDay "5"  (SolvedLong Day5.part1 5197) (SolvedLong Day5.part2 18605)
  solveDay "6"  (Solved Day6.part1 386755)  (Solved Day6.part2 1732731810807)
  solveDay "7"  (Solved Day7.part1 331067)  (Solved Day7.part2 92881128)
  solveDay "8"  (Solved Day8.part1 301)     (Solved Day8.part2 908067)
  solveDay "9"  (Solved Day9.part1 494)     (Solved Day9.part2 1048128)
  solveDay "10" (Solved Day10.part1 345441) (Solved Day10.part2 3235371166)
  solveDay "11" (Solved Day11.part1 1603)   (Solved Day11.part2 222)
  solveDay "12" (Solved Day12.part1 4495)   (Solved Day12.part2 131254)
  solveDay "13" (Solved Day13.part1 747)    (SolvedStringAnswer Day13.part2)
  solveDay "14" (Solved Day14.part1 3697)   (Solved Day14.part2 4371307836157)
  solveDay "15" (Solved Day15.part1 435)    (SolvedLong Day15.part2 2842)
  solveDay "16" (Solved Day16.part1 897)    (Solved Day16.part2 9485076995911) 
  solveDay "17" (Solved Day17.part1 5050)   (Solved Day17.part2 2223)
  solveDay "18" (Solved Day18.part1 2907)   (Solved Day18.part2 4690)
  solveDay "19" (SolvedLong Day19.part1 472) (SolvedLong Day19.part2 12092)

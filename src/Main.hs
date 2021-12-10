module Main where

import Control.Exception (assert)
import Day1
import Day2

main :: IO ()
main = do
  day1input <- readFile "data/1"
  day2input <- readFile "data/2"
  let day1part1Soln = Day1.part1 day1input
      day1part2Soln = Day1.part2 day1input
      day2part1Soln = Day2.part1 day2input
      day2part2Soln = Day2.part2 day2input
  putStrLn "Day 1"
  assert (day1part1Soln == 1451) (print day1part1Soln)
  assert (day1part2Soln == 1395) (print day1part2Soln)
  putStrLn "Day 2"
  assert (day2part1Soln == 1936494)    (print day2part1Soln)
  assert (day2part2Soln == 1997106066) (print day2part2Soln)
  putStrLn ""

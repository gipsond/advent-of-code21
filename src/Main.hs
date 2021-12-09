module Main where

import Control.Exception (assert)
import System.IO

main :: IO ()
main = do
  contents <- readFile "data/1"
  let day1part1Soln = day1part1 contents
      day1part2Soln = day1part2 contents
  assert (day1part1Soln == 1451) (print day1part1Soln)
  assert (day1part2Soln == 1395) (print day1part2Soln)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l = zip l (tail l)

windows :: [a] -> [(a, a, a)]
windows [] = []
windows [head] = []
windows l = zip3 l (tail l) (tail (tail l))

day1parse :: String -> [Int]
day1parse = map read . lines

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . pairs

day1part1 :: String -> Int
day1part1 = countIncreases . day1parse

day1part2 :: String -> Int
day1part2 = countIncreases . map (\(x, y, z) -> x + y + z) . windows . day1parse
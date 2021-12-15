module Day7
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Debug.Trace (traceShow)
import Parsing (commaSeparatedIntList, parseWith)
import Text.Parsec (eof)

-- Parsing

type CrabPosition = Int

parse :: String -> [CrabPosition]
parse = parseWith $ commaSeparatedIntList <* eof

-- Part 1

-- Optimal constant to minimize mean absolute deviation is the median.
-- But a simple search is fast enough.
--
-- median :: [Int] -> Int
-- median l =
--   let sl = sort l
--       len = length sl
--       middleIndex = len `div` 2
--    in if even len
--         then ((sl !! middleIndex) + (sl !! (middleIndex + 1))) `div` 2
--         else sl !! middleIndex

type CostFunction = (CrabPosition -> CrabPosition -> Int)

fuelCost :: CrabPosition -> CrabPosition -> Int
fuelCost p1 p2 = abs $ p1 - p2

totalCost :: CostFunction -> [CrabPosition] -> CrabPosition -> Int
totalCost costF ps alignmentPosition = sum . map (costF alignmentPosition) $ ps

minimumTotalCost :: CostFunction -> [CrabPosition] -> Int
minimumTotalCost f ps =
   minimum . map (totalCost f ps) $ [minimum ps .. maximum ps]

part1 :: String -> Int
part1 = minimumTotalCost fuelCost . parse

-- Part 2

trueFuelCost :: CrabPosition -> CrabPosition -> Int
trueFuelCost p1 p2 =
  let d = abs $ p1 - p2
   in (d * (d + 1)) `div` 2

part2 :: String -> Int
part2 = minimumTotalCost trueFuelCost . parse

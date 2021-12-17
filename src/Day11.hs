module Day11
  ( part1,
    part2,
  )
where

import Parsing (parseDigitGrid)
import Utils (at, neighbors, xMax, yMax, eightDNeighbors)
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Debug.Trace (traceShow)
import Data.List (findIndex)

-- Parsing

type EnergyLevel = Int

type OctopusGrid = [[EnergyLevel]]

parse :: String -> OctopusGrid
parse = parseDigitGrid

-- Part 1

step :: OctopusGrid -> OctopusGrid
step = fromJust . find (not . anyShouldFlash) . iterate flash . increaseEnergy

increaseEnergy :: OctopusGrid -> OctopusGrid
increaseEnergy = map (map (+ 1))

shouldFlash :: EnergyLevel -> Bool
shouldFlash = (> 9)

anyShouldFlash :: OctopusGrid -> Bool
anyShouldFlash = any (any shouldFlash)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

countFlashes :: OctopusGrid -> Int
countFlashes = sum . map (count (== 0))

flash :: OctopusGrid -> OctopusGrid
flash og =
  [ [newValue x y | x <- [0 .. xMax og]]
    | y <- [0 .. yMax og]
  ]
  where
    newValue :: Int -> Int -> EnergyLevel
    newValue x y =
      let v = at og x y
       in if v > 9 || v == 0
            then 0
            else v + (count shouldFlash . map (uncurry . at $ og) $ eightDNeighbors og x y)

part1 :: String -> Int
part1 = sum . map countFlashes . take 101 . iterate step . parse

-- Part 2

allFlash :: OctopusGrid -> Bool
allFlash = all (all (== 0))

part2 :: String -> Int
part2 = fromJust . findIndex allFlash . iterate step . parse
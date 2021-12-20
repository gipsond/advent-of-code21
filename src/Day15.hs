module Day15
  ( part1,
    part2,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (Foldable (foldl'), minimumBy)
import Data.Heap (Entry (payload, priority), Heap)
import qualified Data.Heap as Heap
import Data.List (delete)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Parsing (parseDigitGrid)
import Utils (at, neighbors, xMax, yMax)

-- Parsing

type RiskLevel = Int

type RiskMap = [[Int]]

parse :: String -> RiskMap
parse = parseDigitGrid

-- Part 1

type Point = (Int, Int)

findSafestPath :: RiskMap -> [Point]
findSafestPath rm =
  findSafestPath' (Heap.fromList [Heap.Entry 0 (0, 0)]) (Map.singleton (0, 0) 0) Map.empty
  where
    constructPath :: Map Point Point -> [Point] -> [Point]
    constructPath _ [] = error "called constructPath with no initial point"
    constructPath _ path@((0, 0) : _) = path
    constructPath prev path@(h : t) = constructPath prev ((prev ! h) : path)
    findSafestPath' :: Heap (Heap.Entry Int Point) -> Map Point Int -> Map Point Point -> [Point]
    findSafestPath' q dist prev
      | null q = constructPath prev [(xMax rm, yMax rm)]
      | otherwise =
        let (e, q') = fromJust . Heap.uncons $ q
            Heap.Entry {priority = priority, payload = currentP@(x, y)} = e
            currentRisk = dist ! currentP
            (dist', prev', q'') =
              if priority /= currentRisk
                then (dist, prev, q')
                else
                  foldl'
                    ( \(d, p, q) n@(nx, ny) ->
                        let totalRisk = currentRisk + at rm nx ny
                         in case d !? n of
                              Just risk ->
                                if totalRisk < risk
                                  then (Map.insert n totalRisk d, Map.insert n currentP p, Heap.insert (Heap.Entry totalRisk n) q)
                                  else (d, p, q)
                              Nothing -> (Map.insert n totalRisk d, Map.insert n currentP p, Heap.insert (Heap.Entry totalRisk n) q)
                    )
                    (dist, prev, q')
                    (neighbors rm x y)
         in findSafestPath' q'' dist' prev'

totalRisk :: RiskMap -> [Point] -> Int
totalRisk rm = sum . map (uncurry (at rm))

totalRiskOfShortestPath :: RiskMap -> Int
totalRiskOfShortestPath rm = totalRisk rm . tail . findSafestPath $ rm

part1 :: String -> Int
part1 = totalRiskOfShortestPath . parse

-- Part 2

fullMap :: RiskMap -> RiskMap
fullMap = expandVertical . map expandHorizontal
  where
    expandHorizontal :: [RiskLevel] -> [RiskLevel]
    expandHorizontal = concat . take 5 . iterate (map increaseRisk)
    expandVertical :: RiskMap -> RiskMap
    expandVertical = concat . take 5 . iterate (map (map increaseRisk))
    increaseRisk :: RiskLevel -> RiskLevel
    increaseRisk 9 = 1
    increaseRisk i = i + 1

part2 :: String -> Int
part2 = totalRiskOfShortestPath . fullMap . parse

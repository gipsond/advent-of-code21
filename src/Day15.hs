module Day15
  ( part1,
    part2,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (minimumBy, Foldable (foldl'))
import Data.List (delete)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Heap (Heap, Entry (payload, priority))
import qualified Data.Heap as Heap
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Parsing (parseDigitGrid)
import Utils (at, neighbors, xMax, yMax)
import Data.Maybe (fromJust)

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
            Heap.Entry { priority=priority, payload=currentP@(x, y) } = e
            currentRisk = dist ! currentP
            (dist', prev', q'') =
              if priority /= currentRisk then (dist, prev, q')
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

increaseRisk :: RiskLevel -> RiskLevel
increaseRisk 9 = 1
increaseRisk i = i + 1

totalRisk :: RiskMap -> [Point] -> Int
totalRisk rm = sum . map (uncurry (at rm))

part1 :: String -> Int
part1 s = let
  rm = parse s
  in totalRisk rm . tail . findSafestPath $ rm

-- Part 2

fullMap :: RiskMap -> RiskMap
fullMap = expandVertical .  map expandHorizontal
  where expandHorizontal :: [RiskLevel] -> [RiskLevel]
        expandHorizontal = concat . take 5 . iterate (map increaseRisk)
        expandVertical :: RiskMap -> RiskMap
        expandVertical = concat . take 5 . iterate (map (map increaseRisk))

part2 :: String -> Int
part2 s = let
  rm = fullMap . parse $ s
  in totalRisk rm . tail . findSafestPath $ rm

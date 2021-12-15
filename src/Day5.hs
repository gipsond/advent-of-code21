module Day5
  ( part1,
    part2,
  )
where

import Data.List (intersect, nub)
import Debug.Trace
import Parsing (Parser, comma, parseLinesWith, int)
import Text.Parsec (char, digit, many1, string)
import Control.Monad (liftM2)

-- Parsing

type Point = (Int, Int)

type LineSegment = (Point, Point)

point :: Parser Point
point = do
  x <- int
  comma
  y <- int
  return (x, y)

lineSegment :: Parser LineSegment
lineSegment = do
  p1 <- point
  string " -> "
  p2 <- point
  return (p1, p2)

parse :: String -> [LineSegment]
parse = parseLinesWith lineSegment

-- Part 1

type Range = (Int, Int)

points :: LineSegment -> [Point]
points ls@((x1, y1), (x2, y2))
  | isHorizontal         ls = [(x, y1) | x <- rangeList (x1, x2)]
  | isVertical           ls = [(x1, y) | y <- rangeList (y1, y2)]
  | isDiagonalDescending ls = [(x, y) | x <- rangeList (x1, x2), y <- rangeList (y1, y2), x + y == x1 + y1]
  | otherwise               = [(x, y) | x <- rangeList (x1, x2), y <- rangeList (y1, y2), x - y == x1 - y1]

rangeList :: Range -> [Int]
rangeList (v1, v2)
    | v1 <= v2  = [v1..v2]
    | otherwise = [v2..v1]

isHorizontal :: LineSegment -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

isVertical :: LineSegment -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isHorizontalOrVertical :: LineSegment -> Bool
isHorizontalOrVertical = liftM2 (||) isHorizontal isVertical

pairs :: Eq a => [a] -> [(a, a)]
pairs l = [(e1, e2) | e1 <- l, e2 <- l, e1 /= e2]

dups :: Eq a => [a] -> [a]
dups [] = []
dups (x:xs) = if x `elem` xs then x : dups xs else dups xs

countOverlappingPoints :: [LineSegment] -> Int
countOverlappingPoints = length . nub . dups . (=<<) points

part1 :: String -> Int
part1 = countOverlappingPoints . filter isHorizontalOrVertical . parse

-- Part 2

isDiagonalDescending :: LineSegment -> Bool
isDiagonalDescending ls@((x1, y1), (x2, y2)) =
  let rise = y2 - y1
      run = x2 - x1
  in (not . isHorizontalOrVertical $ ls) && (rise `div` run) < 0

part2 :: String -> Int
part2 = countOverlappingPoints . parse

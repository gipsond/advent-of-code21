module Day17 (
    part1,
    part2
) where
import Parsing (Parser, parseWith, int)
import Text.Parsec (string)
import Debug.Trace (traceShow)
import Control.Monad (liftM2)

-- Parsing

type Range = (Int, Int)
type TargetArea = (Range, Range)

range :: Parser Range
range = do
    low <- int
    string ".."
    high <- int
    return (low, high)

targetArea :: Parser TargetArea
targetArea = do
    string "target area: x="
    xRange <- range
    string ", y="
    yRange <- range
    return (xRange, yRange)

parse :: String -> TargetArea
parse = parseWith targetArea

-- Part 1

inRange :: Range -> Int -> Bool
inRange (low, high) = liftM2 (&&) (>= low) (<= high)

hitsTarget :: TargetArea -> Int -> Int -> Bool
hitsTarget (xr@(_, xMax), yr@(yMin, _)) vx0 vy0 = hitsTarget' (0, 0) (vx0, vy0)
    where hitsTarget' :: (Int, Int) -> (Int, Int) -> Bool
          hitsTarget' (x, y) (vx, vy)
              | inRange xr x && inRange yr y = True
              | x > xMax || y < yMin = False
              | otherwise = hitsTarget' (x + vx, y + vy) (drag vx, vy - 1)
          drag :: Int -> Int
          drag v = case compare v 0 of
              LT -> v + 1
              EQ -> v
              GT -> v - 1

maxHeight :: Int -> Int
maxHeight vy0 = (vy0 * (vy0 + 1)) `div` 2

searchSpace :: TargetArea -> [(Int, Int)]
searchSpace ((_, xMax), (yMin, _)) = [(vx, vy) | vx <- [0..xMax], vy <- [yMin..(-yMin - 1)]]

findBestHeight :: TargetArea -> Int
findBestHeight ta = maximum . map (maxHeight . snd) . filter (uncurry (hitsTarget ta)) . searchSpace $ ta

part1 :: String -> Int
part1 = findBestHeight . parse

-- Part 2

part2 :: String -> Int
part2 s = 
    let ta = parse s
    in length . filter (uncurry (hitsTarget ta)) . searchSpace $ ta

{-# LANGUAGE TupleSections #-}

module Day6 (
    part1,
    part2
)
where
import Parsing (Parser, parseWith, int, comma)
import Text.Parsec (sepBy1, endOfLine, eof)
import Debug.Trace (traceShow)
import Data.List (group, sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Parsing

type LanternfishTimer = Int

timers :: Parser [LanternfishTimer]
timers = do
    ts <- sepBy1 int comma
    endOfLine
    eof
    return ts

parse :: String -> [LanternfishTimer]
parse = parseWith timers

-- Part 1

counts :: [LanternfishTimer] -> IntMap Int
counts l = IntMap.fromListWith (+) (map (, 1) l)

addIn :: Num a => Int -> a -> IntMap a -> IntMap a
addIn = IntMap.insertWith (+)

simulateLanternfish :: IntMap Int -> IntMap Int
simulateLanternfish =
    IntMap.foldlWithKey
        (\acc timer count -> 
            case timer of
                0 -> addIn 6 count . addIn 8 count $ acc
                n -> addIn (n - 1) count acc
        )
        IntMap.empty

sumAfterSimulatingNDays :: Int -> IntMap Int -> Int
sumAfterSimulatingNDays n m = sum $ iterate simulateLanternfish m !! n

part1 :: String -> Int
part1 = sumAfterSimulatingNDays 80 . counts . parse

-- Part 2

part2 :: String -> Int
part2 = sumAfterSimulatingNDays 256 . counts . parse
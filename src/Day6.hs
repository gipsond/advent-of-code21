{-# LANGUAGE TupleSections #-}

module Day6
  ( part1,
    part2,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (group, sort)
import Debug.Trace (traceShow)
import Parsing (Parser, comma, commaSeparatedIntList, int, parseWith)
import Text.Parsec (endOfLine, eof, sepBy1)

-- Parsing

type LanternfishTimer = Int

parse :: String -> [LanternfishTimer]
parse = parseWith $ commaSeparatedIntList <* eof

-- Part 1

counts :: [LanternfishTimer] -> IntMap Int
counts l = IntMap.fromListWith (+) (map (,1) l)

addIn :: Num a => Int -> a -> IntMap a -> IntMap a
addIn = IntMap.insertWith (+)

simulateLanternfish :: IntMap Int -> IntMap Int
simulateLanternfish =
  IntMap.foldlWithKey
    ( \acc timer count ->
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
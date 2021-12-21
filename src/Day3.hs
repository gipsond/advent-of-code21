module Day3 (part1, part2) where

import Control.Monad (liftM2)
import Debug.Trace
import Numeric (readInt)
import Text.Parsec (char, many1, (<|>))
import Parsing (Parser, parseLinesWith, binaryNumber)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Utils (column, binValue, BinaryNumber, Bit (..))

-- Parsing

parse :: String -> [BinaryNumber]
parse = parseLinesWith binaryNumber

-- Part 1

occurrences :: Eq a => [a] -> a -> Int
occurrences l v = length . filter (== v) $ l

gammaRateBit :: [Bit] -> Bit
gammaRateBit l = maximumBy (comparing . occurrences $ l) [Zero, One]

epsilonRateBit :: [Bit] -> Bit
epsilonRateBit l = minimumBy (comparing . occurrences $ l) [Zero, One]

constructColumnWise :: ([Bit] -> Bit) -> [BinaryNumber] -> BinaryNumber
constructColumnWise f l = map (f . flip column l) [0..11]

gammaRate :: [BinaryNumber] -> BinaryNumber
gammaRate = constructColumnWise gammaRateBit

epsilonRate :: [BinaryNumber] -> BinaryNumber
epsilonRate = constructColumnWise epsilonRateBit

part1 :: String -> Int
part1 = multiplyRatings gammaRate epsilonRate . parse

-- Part 2

type Rating = [BinaryNumber] -> BinaryNumber

multiplyRatings :: Rating -> Rating -> [BinaryNumber] -> Int
multiplyRatings r1 r2 = liftM2 (*) (binValue . r1) (binValue . r2)

type BitCriteria = [Bit] -> Bit

applyCriteria :: BitCriteria -> [BinaryNumber] -> BinaryNumber
applyCriteria bc l =
    let 
        f :: Int -> [BinaryNumber] -> BinaryNumber
        f 12 _ = error "Search index overflowed"
        f _ [] = error "None met criteria"
        f _ [one] = one
        f i l = f (i + 1) (filter (\bn -> bn !! i == bc (column i l)) l)
    in
        f 0 l

oxygenGeneratorRating :: [BinaryNumber] -> BinaryNumber
oxygenGeneratorRating = applyCriteria gammaRateBit

co2ScrubberRating :: [BinaryNumber] -> BinaryNumber
co2ScrubberRating = applyCriteria epsilonRateBit

part2 :: String -> Int
part2 = multiplyRatings oxygenGeneratorRating co2ScrubberRating . parse
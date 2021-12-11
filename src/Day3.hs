module Day3 (part1, part2) where

import Control.Monad (liftM2)
import Debug.Trace
import Numeric (readInt)
import Text.Parsec (char, many1, (<|>))
import Parsing (Parser, parseLinesWith)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)

-- Parsing

type BinaryNumber = [Bit]

data Bit
  = Zero
  | One
  deriving (Show, Eq)

bit :: Parser Bit
bit = (char '0' >> return Zero)
  <|> (char '1' >> return One)

binaryNumber :: Parser BinaryNumber
binaryNumber = many1 bit

parse :: String -> [BinaryNumber]
parse = parseLinesWith binaryNumber

-- Part 1

value :: Bit -> Int
value Zero = 0
value One  = 1

toInt :: BinaryNumber -> Int
toInt = foldl (\acc b -> 2 * acc + value b) 0

occurrences :: Eq a => [a] -> a -> Int
occurrences l v = length . filter (== v) $ l

gammaRateBit :: [Bit] -> Bit
gammaRateBit l = maximumBy (comparing . occurrences $ l) [Zero, One]

epsilonRateBit :: [Bit] -> Bit
epsilonRateBit l = minimumBy (comparing . occurrences $ l) [Zero, One]

column :: Int -> [[a]] -> [a]
column i = map (!! i)

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
multiplyRatings r1 r2 = liftM2 (*) (toInt . r1) (toInt . r2)

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
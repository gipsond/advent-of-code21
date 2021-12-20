{-# LANGUAGE TupleSections #-}

module Day14
  ( part1,
    part2,
  )
where

import Data.Foldable (find)
import Data.Map (Map, assocs, (!?))
import qualified Data.Map as Map
import Debug.Trace (traceShow)
import Parsing (Parser, parseWith)
import Text.Parsec (alphaNum, char, endOfLine, eof, many1, string)
import Utils (pairs)

-- Parsing

type Element = Char

type PolymerTemplate = String

element :: Parser Element
element = alphaNum

polymerTemplate :: Parser PolymerTemplate
polymerTemplate = many1 element <* endOfLine

type PairInsertionRule = (Element, Element, Element)

pairInsertionRule :: Parser PairInsertionRule
pairInsertionRule = do
  first <- element
  second <- element
  string " -> "
  toInsert <- element
  endOfLine
  return (first, second, toInsert)

pairInsertionRules :: Parser [PairInsertionRule]
pairInsertionRules = many1 pairInsertionRule

polymerTemplateAndPairInsertionRules :: Parser (PolymerTemplate, [PairInsertionRule])
polymerTemplateAndPairInsertionRules = do
  pt <- polymerTemplate
  endOfLine
  pis <- pairInsertionRules
  eof
  return (pt, pis)

parse :: String -> (PolymerTemplate, [PairInsertionRule])
parse = parseWith polymerTemplateAndPairInsertionRules

-- Part 1

applyInsertionRules :: [PairInsertionRule] -> Map (Element, Element) Int -> Map (Element, Element) Int
applyInsertionRules pis m =
  foldl
    ( \newMap (first, second, toInsert) ->
        let pair = (first, second)
         in case m !? pair of
              Nothing -> newMap
              Just count ->
                    Map.insertWith (+) (first, toInsert) count
                  . Map.insertWith (+) (toInsert, second) count
                  . Map.adjust (\i -> i - count) pair
                  $ newMap
    )
    m
    pis

counts :: Ord a => [a] -> Map a Int
counts l = Map.fromListWith (+) (map (,1) l)

part1 :: String -> Int
part1 = runPolymerizationAndCount 10 . parse

elementCounts :: Map (Element, Element) Int -> Map Element Int
elementCounts =
      Map.map (\c -> c `div` 2 + c `mod` 2)
    . Map.fromListWith (+)
    . concatMap (\((e1, e2), c) -> [(e1, c), (e2, c)])
    . assocs

-- Part 2

runPolymerizationAndCount :: Int -> (PolymerTemplate, [PairInsertionRule]) -> Int
runPolymerizationAndCount steps (pt, pis) =
  let eCounts = elementCounts . (!! steps) . iterate (applyInsertionRules pis) . counts . pairs $ pt
   in maximum eCounts - minimum eCounts

part2 :: String -> Int
part2 = runPolymerizationAndCount 40 . parse
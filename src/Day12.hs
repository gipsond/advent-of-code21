module Day12
  ( part1,
    part2,
  )
where

import Control.Monad (void)
import Data.Char (isLower)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Parsing (Parser, parseLinesWith)
import Text.Parsec (alphaNum, char, many1)

-- Parsing

type AdjacencyList = [(String, String)]

type Graph = Map String (Set String)

caveName :: Parser String
caveName = many1 alphaNum

caveConnection :: Parser (String, String)
caveConnection = do
  c1 <- caveName
  void $ char '-'
  c2 <- caveName
  return (c1, c2)

parse :: String -> AdjacencyList
parse = parseLinesWith caveConnection

-- Part 1

type Path = [String]

isSmall :: String -> Bool
isSmall = all isLower

allCaves :: AdjacencyList -> Set String
allCaves = Set.fromList . concatMap (\(to, from) -> [to, from])

neighbors :: AdjacencyList -> String -> Set String
neighbors al s = Set.delete s . allCaves . filter (\(to, from) -> to == s || from == s) $ al

graph :: AdjacencyList -> Graph
graph al = Map.fromSet (neighbors al) (allCaves al)

findPaths :: Bool -> Graph -> [Path]
findPaths secondVisitAllowed g =
  let f :: Bool -> Path -> Set String -> String -> [Path]
      f sva p vis s
        | s == "start" || (not sva && Set.member s vis) = []
        | s == "end" = [p ++ ["end"]]
        | otherwise =
            let sva' = sva && not (Set.member s vis)
                p' = p ++ [s]
                vis' = if isSmall s then Set.insert s vis else vis
                ns = Set.toList $ g ! s
            in concatMap (f sva' p' vis') ns
   in concatMap (f secondVisitAllowed ["start"] Set.empty) (g ! "start")

part1 :: String -> Int
part1 = length . findPaths False . graph . parse

-- Part 2

part2 :: String -> Int
part2 = length . findPaths True . graph . parse

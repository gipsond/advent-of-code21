module Main where

import Control.Exception (assert)
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import System.IO
import Text.Parsec (Parsec, ParsecT, digit, endOfLine, many1, parse, parsecMap, space, string, (<|>))
import Debug.Trace

main :: IO ()
main = do
  day1input <- readFile "data/1"
  day2input <- readFile "data/2"
  let day1part1Soln = day1part1 day1input
      day1part2Soln = day1part2 day1input
      day2part1Soln = day2part1 day2input
      day2part2Soln = day2part2 day2input
  putStrLn "Day 1"
  assert (day1part1Soln == 1451) (print day1part1Soln)
  assert (day1part2Soln == 1395) (print day1part2Soln)
  putStrLn "Day 2"
  assert (day2part1Soln == 1936494)    (print day2part1Soln)
  assert (day2part2Soln == 1997106066) (print day2part2Soln)
  putStrLn ""

-- Day 1

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l = zip l (tail l)

windows :: [a] -> [(a, a, a)]
windows [] = []
windows [head] = []
windows l = zip3 l (tail l) (tail (tail l))

day1parse :: String -> [Int]
day1parse = map read . lines

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . pairs

day1part1 :: String -> Int
day1part1 = countIncreases . day1parse

day1part2 :: String -> Int
day1part2 = countIncreases . map (\(x, y, z) -> x + y + z) . windows . day1parse

-- Day 2

type Parser = Parsec String ()

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving Show

commandDirection :: Parser (Int -> Command)
commandDirection =
        (string "forward" >> return Forward)
    <|> (string "down"    >> return Down)
    <|> (string "up"      >> return Up)

command :: Parser Command
command = do
  cd <- commandDirection
  void $ many1 space
  i <- many1 digit
  return . cd . read $ i

expectRight :: Show a => Either a b -> b
expectRight (Left err) =  error . show $ err
expectRight (Right v) = v

day2lineparse :: String -> Command
day2lineparse = expectRight . parse command ""

day2parse :: String -> [Command]
day2parse = map day2lineparse . lines

type Position = (Int, Int)

updatePosition :: Position -> Command -> Position
updatePosition (h, d) (Forward n) = (h + n, d)
updatePosition (h, d) (Down n)    = (h, d + n)
updatePosition (h, d) (Up n)      = (h, d - n)

day2part1 :: String -> Int
day2part1 s =
  let
    (finalH, finalD) = foldl updatePosition (0, 0) (day2parse s)
  in
    finalH * finalD

type State = (Int, Int, Int)

updateState :: State -> Command -> State
updateState (h, d, a) (Forward n) = (h + n, d + a * n, a)
updateState (h, d, a) (Down n)    = (h, d, a + n)
updateState (h, d, a) (Up n)      = (h, d, a - n)

day2part2 :: String -> Int
day2part2 s =
  let
    (finalH, finalD, _) = foldl updateState (0, 0, 0) (day2parse s)
  in
    finalH * finalD

-- Day 3


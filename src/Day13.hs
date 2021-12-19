module Day13
  ( part1,
    part2,
  )
where

import Data.Bifunctor (Bifunctor (second), first)
import Data.List (nub, intersperse, intercalate)
import Debug.Trace (traceShow)
import Parsing (Parser, comma, int, parseWith)
import Text.Parsec (char, endOfLine, eof, string, (<|>))
import Text.Parsec.Combinator (many1)

-- Parsing

type Point = (Int, Int)

type FoldInstruction = (FoldDirection, Int)

data FoldDirection = Horizontal | Vertical deriving (Show)

point :: Parser Point
point = do
  x <- int
  comma
  y <- int
  endOfLine
  return (x, y)

points :: Parser [Point]
points = many1 point

foldDirection :: Parser FoldDirection
foldDirection =
  (char 'x' >> return Vertical)
    <|> (char 'y' >> return Horizontal)

foldInstruction :: Parser FoldInstruction
foldInstruction = do
  string "fold along "
  fd <- foldDirection
  string "="
  coord <- int
  endOfLine
  return (fd, coord)

foldInstructions :: Parser [FoldInstruction]
foldInstructions = many1 foldInstruction

pointsAndFoldInstructions :: Parser ([Point], [FoldInstruction])
pointsAndFoldInstructions = do
  ps <- points
  endOfLine
  fis <- foldInstructions
  eof
  return (ps, fis)

parse :: String -> ([Point], [FoldInstruction])
parse = parseWith pointsAndFoldInstructions

-- Part 1

foldPoints :: FoldInstruction -> [Point] -> [Point]
foldPoints (fd, i) =
  let foldCoord v = if v < i then v else 2 * i - v
      (getCoord, mapCoord) = case fd of
        Vertical -> (fst, first)
        Horizontal -> (snd, second)
   in nub . filter ((/= i) . getCoord) . map (mapCoord foldCoord)

part1 :: String -> Int
part1 s =
  let (ps, firstInstruction : _) = parse s
   in length . foldPoints firstInstruction $ ps

-- Part 2

displayPoints :: [Point] -> String
displayPoints ps = let
    (xs, ys) = unzip ps
    maxX = maximum xs
    maxY = maximum ys
    rows = [[if (x, y) `elem` ps then '#' else '.' | x <- [0..maxX]] | y <- [0..maxY]]
    in intercalate "\n" rows 

part2 :: String -> String
part2 s = 
  let (ps, fis) = parse s
   in displayPoints $ foldl (flip foldPoints) ps fis
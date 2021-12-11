module Day2
  ( part1,
    part2,
  )
where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Parsing (Parser, parseLinesWith)
import Text.Parsec (digit, many1, space, string, (<|>))

-- Parsing

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

commandDirection :: Parser (Int -> Command)
commandDirection =
  (string "forward" >> return Forward)
    <|> (string "down" >> return Down)
    <|> (string "up" >> return Up)

command :: Parser Command
command = do
  cd <- commandDirection
  void $ many1 space
  i <- many1 digit
  return . cd . read $ i

parse :: String -> [Command]
parse = parseLinesWith command

-- Part 1

type Position = (Int, Int)

updatePosition :: Position -> Command -> Position
updatePosition (h, d) (Forward n) = (h + n, d)
updatePosition (h, d) (Down n) = (h, d + n)
updatePosition (h, d) (Up n) = (h, d - n)

part1 :: String -> Int
part1 s =
  let (finalH, finalD) = foldl updatePosition (0, 0) (parse s)
   in finalH * finalD

-- Part 2

type State = (Int, Int, Int)

updateState :: State -> Command -> State
updateState (h, d, a) (Forward n) = (h + n, d + a * n, a)
updateState (h, d, a) (Down n) = (h, d, a + n)
updateState (h, d, a) (Up n) = (h, d, a - n)

part2 :: String -> Int
part2 s =
  let (finalH, finalD, _) = foldl updateState (0, 0, 0) (parse s)
   in finalH * finalD

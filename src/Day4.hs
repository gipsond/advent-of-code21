module Day4
  ( part1,
    part2,
  )
where

import Control.Monad (liftM2)
import Data.Foldable (find)
import Data.List (delete, (\\))
import Debug.Trace
import Parsing (Parser, parseWith, comma)
import Text.Parsec (char, digit, endOfLine, eof, many, many1, parsecMap, sepBy1, sepEndBy1, space)
import Utils (column)

-- Parsing

data BingoSpace = BingoSpace
  { marked :: Bool,
    number :: BingoNumber
  }
  deriving (Show, Eq)

type BingoNumber = Int

type BingoDraws = [BingoNumber]

type BingoRow = [BingoSpace]

type BingoBoard = [BingoRow]

bingoNumber :: Parser BingoNumber
bingoNumber = parsecMap read $ many (char ' ') >> many1 digit

bingoDraws :: Parser BingoDraws
bingoDraws = (bingoNumber `sepBy1` comma) <* endOfLine

unmarkedSpace :: BingoNumber -> BingoSpace
unmarkedSpace n = BingoSpace {marked = False, number = n}

bingoRow :: Parser BingoRow
bingoRow = do
  numbers <- many1 bingoNumber
  endOfLine
  return . map unmarkedSpace $ numbers

bingoBoard :: Parser BingoBoard
bingoBoard = many1 bingoRow

input :: Parser (BingoDraws, [BingoBoard])
input = do
  draws <- bingoDraws
  endOfLine
  boards <- sepEndBy1 bingoBoard endOfLine
  eof
  return (draws, boards)

parse :: String -> (BingoDraws, [BingoBoard])
parse = parseWith input

-- Part 1

markSpace :: BingoNumber -> BingoSpace -> BingoSpace
markSpace toMark BingoSpace {marked = m, number = n} =
  BingoSpace {marked = m || toMark == n, number = n}

markRow :: BingoNumber -> BingoRow -> BingoRow
markRow n = map (markSpace n)

mark :: BingoNumber -> BingoBoard -> BingoBoard
mark n = map (markRow n)

hasWinningRow :: BingoBoard -> Bool
hasWinningRow = any (all marked)

hasWinningColumn :: BingoBoard -> Bool
hasWinningColumn board = any (\i -> all marked (column i board)) [0 .. 4]

at :: Int -> Int -> BingoBoard -> BingoSpace
at x y b = b !! y !! x

isWinning :: BingoBoard -> Bool
isWinning = liftM2 (||) hasWinningColumn hasWinningRow

findFirstWinner :: BingoDraws -> [BingoBoard] -> (BingoNumber, BingoBoard)
findFirstWinner [] bs = error "No winning boards after trying all numbers"
findFirstWinner (n : ns) bs =
  let markedBs = map (mark n) bs
   in case find isWinning markedBs of
        Just b -> (n, b)
        Nothing -> findFirstWinner ns markedBs

score :: BingoNumber -> BingoBoard -> Int
score winningNumber b =
  let s = sum . map number . filter (not . marked) . concat $ b
   in s * winningNumber

part1 :: String -> Int
part1 = uncurry score . uncurry findFirstWinner . parse

-- Part 2

findLastWinner :: BingoDraws -> [BingoBoard] -> (BingoNumber, BingoBoard)
findLastWinner [] bs = error "Not all boards winning after trying all numbers"
findLastWinner (n : ns) bs =
  let markedBs = map (mark n) bs
      winningBs = filter isWinning markedBs
   in case (markedBs, winningBs) of
        ([], _) -> error "Out of boards but didn't return one"
        ([mb], [wb]) -> (n, wb)
        (mbs, wbs) -> findLastWinner ns (mbs \\ wbs)

part2 :: String -> Int
part2 = uncurry score . uncurry findLastWinner . parse

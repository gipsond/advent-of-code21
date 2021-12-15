module Day8
  ( part1,
    part2,
  )
where

import Data.Foldable (find)
import Data.Functor (void, (<&>))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Parsing (Parser, parseLinesWith, sepTry1)
import Text.Parsec (char, many1, sepBy1, string, (<|>))

-- Parsing

data SignalWire
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Ord, Show)

signalWire :: Parser SignalWire
signalWire =
  (char 'a' >> return A)
    <|> (char 'b' >> return B)
    <|> (char 'c' >> return C)
    <|> (char 'd' >> return D)
    <|> (char 'e' >> return E)
    <|> (char 'f' >> return F)
    <|> (char 'g' >> return G)

type SignalPattern = Set SignalWire

data Entry = Entry
  { signalPatterns :: [SignalPattern],
    outputValue :: [SignalPattern]
  }
  deriving (Show)

signalPattern :: Parser SignalPattern
signalPattern = many1 signalWire <&> Set.fromList

signalPatternsP :: Parser [SignalPattern]
signalPatternsP = sepTry1 signalPattern (char ' ')

entry :: Parser Entry
entry = do
  sps <- signalPatternsP
  void $ string " | "
  ov <- signalPatternsP
  return Entry {signalPatterns = sps, outputValue = ov}

parse :: String -> [Entry]
parse = parseLinesWith entry

-- Part 1

part1 :: String -> Int
part1 = length . filter (\ov -> Set.size ov `elem` [2, 4, 3, 7]) . (=<<) outputValue . parse

-- Part 2

data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

value :: Digit -> Int
value Zero = 0
value One = 1
value Two = 2
value Three = 3
value Four = 4
value Five = 5
value Six = 6
value Seven = 7
value Eight = 8
value Nine = 9

toDecimal :: [Digit] -> Int
toDecimal = foldl (\acc i -> acc * 10 + i) 0 . map value

findPatternWithNWires :: Int -> [SignalPattern] -> SignalPattern
findPatternWithNWires n = fromJust . find ((== n) . Set.size)

intOutputValue :: Entry -> Int
intOutputValue Entry {signalPatterns = sps, outputValue = ov} =
  let one = findPatternWithNWires 2 sps
      four = findPatternWithNWires 4 sps
      f :: SignalPattern -> Digit
      f s = case Set.size s of
        2 -> One
        4 -> Four
        7 -> Eight
        3 -> Seven
        5 -> if one `Set.isSubsetOf` s then Three
             else if (== 3) . Set.size $ Set.intersection s four then Five
             else Two
        6 -> if not $ one `Set.isSubsetOf` s then Six
             else if four `Set.isSubsetOf` s then Nine
             else Zero
        _ -> error "invalid input pattern"
   in toDecimal . map f $ ov

part2 :: String -> Int
part2 = sum . map intOutputValue . parse
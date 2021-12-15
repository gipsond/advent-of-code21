module Parsing
  ( comma,
    Parser,
    parseLines,
    parseLinesWith,
    parseWith,
    int,
    commaSeparatedIntList,
    sepTry,
    sepTry1,
  )
where

import Text.Parsec (Parsec, parse, char, digit, many1, sepBy1, endOfLine, eof, try, many, (<|>))
import Data.Functor ((<&>))
import Control.Applicative (liftA2)

type Parser = Parsec String ()

expectRight :: Show a => Either a b -> b
expectRight (Left err) = error . show $ err
expectRight (Right v) = v

parseWith :: Parser a -> String -> a
parseWith parser = expectRight . parse parser ""

parseLines :: (String -> a) -> String -> [a]
parseLines parseLine = map parseLine . lines

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith = parseLines . parseWith

comma :: Parser Char
comma = char ','

int :: Parser Int
int = many1 digit <&> read

commaSeparatedIntList :: Parser [Int]
commaSeparatedIntList = sepBy1 int comma <* endOfLine

sepTry :: Parser a -> Parser b -> Parser [a]
sepTry p sep = sepTry1 p sep <|> pure []

sepTry1 :: Parser a -> Parser b -> Parser [a]
sepTry1 p sep = liftA2 (:) p (many (try $ sep *> p))
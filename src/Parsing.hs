module Parsing
  ( comma,
    Parser,
    parseLines,
    parseLinesWith,
    parseWith,
    int,
  )
where

import Text.Parsec (Parsec, parse, char, digit, many1)
import Data.Functor ((<&>))

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

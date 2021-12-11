module Parsing
  ( Parser,
    parseLines,
    parseLinesWith,
  )
where

import Text.Parsec (Parsec, parse)

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

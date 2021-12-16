module Day10
  ( part1,
    part2,
  )
where
import Data.Maybe (mapMaybe)
import Data.Either (fromRight, isRight, rights, lefts)
import qualified Data.List as List

-- Part 1

syntaxErrorScore :: Char -> Int
syntaxErrorScore ')' = 3
syntaxErrorScore ']' = 57
syntaxErrorScore '}' = 1197
syntaxErrorScore '>' = 25137
syntaxErrorScore _ = error "Unexpected unexpected character"

match :: Char -> Maybe Char
match '(' = Just ')'
match '[' = Just ']'
match '{' = Just '}'
match '<' = Just '>'
match _ = Nothing

firstIllegalChar :: String -> Either [Char] Char
firstIllegalChar s =
  let f :: [Char] -> String -> Either [Char] Char
      f os [] = Left os
      f os (c : cs) =
        if length os > 1 && head os == c
          then f (tail os) cs
          else case match c of
            Just m -> f (m : os) cs
            Nothing -> Right c
   in f [] s

part1 :: String -> Int
part1 = sum . map syntaxErrorScore . rights . map firstIllegalChar . lines

-- Part 2

median :: [Int] -> Int
median l =
  let sl = List.sort l
      len = length sl
      middleIndex = len `div` 2
   in if even len
        then ((sl !! middleIndex) + (sl !! (middleIndex + 1))) `div` 2
        else sl !! middleIndex

autocompleteScore :: [Char] -> Int
autocompleteScore =
    foldl (\totalScore c -> totalScore * 5 + pointValue c) 0
    where pointValue :: Char -> Int
          pointValue ')' = 1
          pointValue ']' = 2
          pointValue '}' = 3
          pointValue '>' = 4
          pointValue _ = error "Unexpected autocomplete score char"


part2 :: String -> Int
part2 = median . map autocompleteScore . lefts . map firstIllegalChar . lines


module Day18
  ( part1,
    part2,
  )
where

import Data.Maybe (isJust)
import Debug.Trace (traceShow)
import Parsing (Parser, int, parseLinesWith)
import Text.Parsec (char, (<|>))
import Data.Complex ()
import Utils (combos)

-- Parsing

type SnailfishNum = (SnailfishNumElem, SnailfishNumElem)

data SnailfishNumElem
  = RegularNumber Int
  | SnailfishNumber SnailfishNum
  deriving (Show, Eq)

snailfishNumElem :: Parser SnailfishNumElem
snailfishNumElem =
  SnailfishNumber <$> snailfishNum
    <|> RegularNumber <$> int

snailfishNum :: Parser SnailfishNum
snailfishNum = do
  char '['
  f <- snailfishNumElem
  char ','
  s <- snailfishNumElem
  char ']'
  return (f, s)

parse :: String -> [SnailfishNum]
parse = parseLinesWith snailfishNum

-- Part 1

isRegularPair :: SnailfishNum -> Bool
isRegularPair (RegularNumber _, RegularNumber _) = True
isRegularPair _ = False

addRight :: SnailfishNumElem -> Int -> SnailfishNumElem
addRight (RegularNumber v) n = RegularNumber (v + n)
addRight (SnailfishNumber (l, r)) n = SnailfishNumber (l, addRight r n)

addLeft :: SnailfishNumElem -> Int -> SnailfishNumElem
addLeft (RegularNumber v) n = RegularNumber (v + n)
addLeft (SnailfishNumber (l, r)) n = SnailfishNumber (addLeft l n, r)

explodeFirst :: SnailfishNum -> (Bool, SnailfishNum)
explodeFirst sn = case f (SnailfishNumber sn) 0 of
  (_, RegularNumber _) -> error "subroutine returned regular number instead of pair"
  (expData, SnailfishNumber sn') -> (isJust expData, sn')
  where
    f :: SnailfishNumElem -> Int -> (Maybe (Maybe Int, Maybe Int), SnailfishNumElem)
    f rn@(RegularNumber _) _ = (Nothing, rn)
    f sn@(SnailfishNumber (RegularNumber ln, RegularNumber rn)) depth =
      if depth >= 4
        then (Just (Just ln, Just rn), RegularNumber 0)
        else (Nothing, sn)
    f sn@(SnailfishNumber (l, r)) depth =
      let (expL, l') = f l (depth + 1)
       in case expL of
            Nothing ->
              let (expR, r') = f r (depth + 1)
               in case expR of
                    Nothing -> (Nothing, sn)
                    Just (addL, addR) ->
                      case addL of
                        Nothing -> (expR, SnailfishNumber (l, r'))
                        Just n -> (Just (Nothing, addR), SnailfishNumber (addRight l n, r'))
            Just (addL, addR) ->
              case addR of
                Nothing -> (expL, SnailfishNumber (l', r))
                Just n -> (Just (addL, Nothing), SnailfishNumber (l', addLeft r n))

explodeAll :: SnailfishNum -> SnailfishNum
explodeAll = applyUntilFalseFlag explodeFirst

splitFirst :: SnailfishNum -> (Bool, SnailfishNum)
splitFirst sn = case f (SnailfishNumber sn) of
  (_, RegularNumber _) -> error "subroutine returned regular number instead of pair"
  (split, SnailfishNumber sn') -> (split, sn')
  where
    f :: SnailfishNumElem -> (Bool, SnailfishNumElem)
    f e@(RegularNumber n) =
      if n >= 10
        then
          let l = n `div` 2
              r = n `div` 2 + n `mod` 2
           in (True, SnailfishNumber (RegularNumber l, RegularNumber r))
        else (False, e)
    f e@(SnailfishNumber (l, r)) =
      case f l of
        (True, l') -> (True, SnailfishNumber (l', r))
        (False, _) -> case f r of
          (True, r') -> (True, SnailfishNumber (l, r'))
          (False, _) -> (False, e)

applyUntilFalseFlag :: (a -> (Bool, a)) -> a -> a
applyUntilFalseFlag f v = applyUntilFalseFlag' f (True, v)
  where
    applyUntilFalseFlag' :: (a -> (Bool, a)) -> (Bool, a) -> a
    applyUntilFalseFlag' f (False, v) = v
    applyUntilFalseFlag' f (True, v) = applyUntilFalseFlag' f . f $ v

reduce :: SnailfishNum -> SnailfishNum
reduce = applyUntilFalseFlag (splitFirst . explodeAll)

add :: SnailfishNum -> SnailfishNum -> SnailfishNum
add a b = reduce (SnailfishNumber a, SnailfishNumber b)

magnitude :: SnailfishNum -> Int
magnitude = magnitude' . SnailfishNumber
    where
    magnitude' :: SnailfishNumElem -> Int
    magnitude' (RegularNumber n) = n
    magnitude' (SnailfishNumber (l, r)) = 3 * magnitude' l + 2 * magnitude' r

part1 :: String -> Int
part1 = magnitude . foldl1 add . parse

-- Part 2

part2 :: String -> Int
part2 = maximum . map (magnitude . uncurry add) . combos . parse

module Utils
  ( column,
    xMax,
    yMax,
    neighbors,
    eightDNeighbors,
    at,
    pairs,
    Bit (..),
    BinaryNumber (..),
    bitValue,
    binValue,
  )
where

import Data.Maybe (catMaybes)

-- General utilities

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l = zip l (tail l)

-- Bit utilities

type BinaryNumber = [Bit]

data Bit
  = Zero
  | One
  deriving (Show, Eq)

bitValue :: Bit -> Int
bitValue Zero = 0
bitValue One  = 1

binValue :: BinaryNumber -> Int
binValue = foldl (\acc b -> 2 * acc + bitValue b) 0

-- 2D List matrix utilities

column :: Int -> [[a]] -> [a]
column i = map (!! i)

xMax :: [[a]] -> Int
xMax = flip (-) 1 . length . head

yMax :: [[a]] -> Int
yMax = flip (-) 1 . length

neighbors :: [[a]] -> Int -> Int -> [(Int, Int)]
neighbors hm x y =
  catMaybes
    [ if 0 < x then Just (x - 1, y) else Nothing,
      if x < xMax hm then Just (x + 1, y) else Nothing,
      if 0 < y then Just (x, y - 1) else Nothing,
      if y < yMax hm then Just (x, y + 1) else Nothing
    ]

diagonalNeighbors :: [[a]] -> Int -> Int -> [(Int, Int)]
diagonalNeighbors hm x y =
  let xNotLow = 0 < x
      xNotHigh = x < xMax hm
      yNotLow = 0 < y
      yNotHigh = y < yMax hm
   in catMaybes
        [ if xNotLow && yNotLow then Just (x - 1, y - 1) else Nothing,
          if xNotHigh && yNotHigh then Just (x + 1, y + 1) else Nothing,
          if xNotHigh && yNotLow then Just (x + 1, y - 1) else Nothing,
          if xNotLow && yNotHigh then Just (x - 1, y + 1) else Nothing
        ]

eightDNeighbors :: [[a]] -> Int -> Int -> [(Int, Int)]
eightDNeighbors hm x y = diagonalNeighbors hm x y ++ neighbors hm x y

at :: [[a]] -> Int -> Int -> a
at hm x y = hm !! y !! x

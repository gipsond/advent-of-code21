module Day9
  ( part1,
    part2,
  )
where

import Control.Monad (liftM2)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Parsing (Parser, parseLinesWith)
import Text.Parsec (digit)
import Text.Parsec.Combinator (many1)

-- Parsing

type Heightmap = [[Height]]

height :: Parser Height
height = digit <&> (read . pure)

heightmapRow :: Parser [Height]
heightmapRow = many1 height

parse :: String -> Heightmap
parse = parseLinesWith heightmapRow

-- Part 1

type Height = Int

type RiskLevel = Int

riskLevel :: Height -> RiskLevel
riskLevel = (+) 1

xMax :: Heightmap -> Int
xMax = flip (-) 1 . length . head

yMax :: Heightmap -> Int
yMax = flip (-) 1 . length

neighbors :: Heightmap -> Int -> Int -> [(Int, Int)]
neighbors hm x y =
  catMaybes
    [ if 0 < x then Just (x - 1, y) else Nothing,
      if x < xMax hm then Just (x + 1, y) else Nothing,
      if 0 < y then Just (x, y - 1) else Nothing,
      if y < yMax hm then Just (x, y + 1) else Nothing
    ]

at :: Heightmap -> Int -> Int -> Height
at hm x y = hm !! y !! x

isLowPoint :: Heightmap -> Int -> Int -> Bool
isLowPoint hm x y = all (\(nx, ny) -> at hm nx ny > at hm x y) $ neighbors hm x y

lowPoints :: Heightmap -> [(Int, Int)]
lowPoints hm = [(x, y) | x <- [0 .. xMax hm], y <- [0 .. yMax hm], isLowPoint hm x y]

part1 :: String -> Int
part1 s =
  let hm = parse s
   in sum . map (riskLevel . uncurry (at hm)) . lowPoints $ hm

-- Part 2

basin :: Heightmap -> Int -> Int -> Set (Int, Int)
basin hm x y =
  dfs Set.empty x y
  where
    height9 :: (Int, Int) -> Bool
    height9 = (== 9) . uncurry (at hm)
    dfs :: Set (Int, Int) -> Int -> Int -> Set (Int, Int)
    dfs vis x y =
      let vis' = Set.insert (x, y) vis
       in foldl
            ( \acc (nx, ny) ->
                if (nx, ny) `elem` acc || height9 (nx, ny)
                  then acc
                  else dfs acc nx ny
            )
            vis'
            $ neighbors hm x y

part2 :: String -> Int
part2 s =
  let hm = parse s
   in product . take 3 . reverse . List.sort . map (length . uncurry (basin hm)) . lowPoints $ hm

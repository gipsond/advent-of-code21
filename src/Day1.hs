module Day1
  ( part1,
    part2,
  )
where

-- Day 1

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l = zip l (tail l)

windows :: [a] -> [(a, a, a)]
windows [] = []
windows [head] = []
windows l = zip3 l (tail l) (tail (tail l))

parse :: String -> [Int]
parse = map read . lines

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . pairs

part1 :: String -> Int
part1 = countIncreases . parse

part2 :: String -> Int
part2 = countIncreases . map (\(x, y, z) -> x + y + z) . windows . parse

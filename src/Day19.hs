{-# LANGUAGE TupleSections #-}

module Day19
  ( part1,
    part2,
  )
where

import Data.Bifunctor (second)
import Data.List (find, foldl', nub, sort)
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
import Debug.Trace (traceShow)
import Parsing (Parser, int, parseWith)
import Text.Parsec (char, endOfLine, eof, many1, sepBy1, string)
import Utils (combos)

-- Parsing

type Coord = (Int, Int, Int)

coord :: Parser Coord
coord = do
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  endOfLine
  return (x, y, z)

scanner :: Parser [Coord]
scanner = do
  string "--- scanner "
  int
  string " ---"
  endOfLine
  many1 coord

scanners :: Parser [[Coord]]
scanners = sepBy1 scanner endOfLine <* eof

parse :: String -> [[Coord]]
parse = parseWith scanners

-- Part 1

numMatches :: Eq a => [a] -> [a] -> Int
numMatches a b = length . filter (`elem` b) . nub $ a

difference :: Coord -> Coord -> Translation
difference (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

translate :: Translation -> Coord -> Coord
translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

type Translation = (Int, Int, Int)

type Rotation = ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))

dotProduct :: Coord -> Coord -> Int
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

composeRot :: Rotation -> Rotation -> Rotation
composeRot (r1, r2, r3) ((v11, v12, v13), (v21, v22, v23), (v31, v32, v33)) =
  let c1 = (v11, v21, v31)
      c2 = (v12, v22, v32)
      c3 = (v13, v23, v33)
   in ( (dotProduct r1 c1, dotProduct r1 c2, dotProduct r1 c3),
        (dotProduct r2 c1, dotProduct r2 c2, dotProduct r2 c3),
        (dotProduct r3 c1, dotProduct r3 c2, dotProduct r3 c3)
      )

rotate :: Rotation -> Coord -> Coord
rotate (r1, r2, r3) c = (dotProduct r1 c, dotProduct r2 c, dotProduct r3 c)

composeRot3 :: Rotation -> Rotation -> Rotation -> Rotation
composeRot3 r1 r2 r3 = composeRot r1 $ composeRot r2 r3

idTl :: Translation
idTl = (0, 0, 0)

idRot :: Rotation
idRot =
  ( (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1)
  )

idTransform :: Transform
idTransform = (idRot, idTl)

xRot :: Rotation
xRot =
  ( (1, 0, 0),
    (0, 0, -1),
    (0, 1, 0)
  )

yRot :: Rotation
yRot =
  ( (0, 0, 1),
    (0, 1, 0),
    (-1, 0, 0)
  )

zRot :: Rotation
zRot =
  ( (0, -1, 0),
    (1, 0, 0),
    (0, 0, 1)
  )

allAxisRots :: Rotation -> [Rotation]
allAxisRots r = take 4 . iterate (composeRot r) $ idRot

allRots :: [Rotation]
allRots = nub $ composeRot3 <$> allAxisRots xRot <*> allAxisRots yRot <*> allAxisRots zRot

type Transform = (Rotation, Translation)

transform :: Transform -> Coord -> Coord
transform (rot, tl) = translate tl . rotate rot

findScannerTransform :: [Coord] -> [Coord] -> Maybe (Rotation, Translation)
findScannerTransform base toTransform =
  let possibleTransforms =
        concatMap
          ( \rot ->
              zip (repeat rot) (difference <$> base <*> map (rotate rot) toTransform)
          )
          allRots
   in find
        ( \tf ->
            let transformed = transform tf <$> toTransform
             in numMatches base transformed >= 12
        )
        possibleTransforms

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe p =
  foldl'
    ( \(justs, nos) e ->
        case p e of
          Just j -> (j : justs, nos)
          Nothing -> (justs, e : nos)
    )
    ([], [])

findAllTransforms :: [Coord] -> [[Coord]] -> [([Coord], [Transform])]
findAllTransforms base = f [(base, [idTransform])]
  where
    f :: [([Coord], [Transform])] -> [[Coord]] -> [([Coord], [Transform])]
    f bases [] = bases
    f bases others =
      let (newBases, others') = partitionMaybe (findCompoundTransform bases) others
       in f (bases ++ newBases) others'
    findCompoundTransform :: [([Coord], [Transform])] -> [Coord] -> Maybe ([Coord], [Transform])
    findCompoundTransform bases toTransform =
      fmap (toTransform,)
        . listToMaybe
        . mapMaybe
          ( \(base, baseTfs) ->
              (: baseTfs) <$> findScannerTransform base toTransform
          )
        $ bases

composeTransform :: Coord -> [Transform] -> Coord
composeTransform = foldl' (flip transform)

findAllBeacons :: [Coord] -> [[Coord]] -> [Coord]
findAllBeacons base scs =
  nub
    . concatMap
      ( \(coords, tfs) ->
          map (`composeTransform` tfs) coords
      )
    $ findAllTransforms base scs

part1 :: String -> Int
part1 s =
  let base : scs = parse s
   in length $ findAllBeacons base scs

-- Part 2

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = sum . map abs $ [x1 - x2, y1 - y2, z1 - z2]

part2 :: String -> Int
part2 s =
  let base : scs = parse s
      scanners = map (composeTransform (0, 0, 0) . snd) $ findAllTransforms base scs
   in maximum $ manhattanDistance <$> scanners <*> scanners


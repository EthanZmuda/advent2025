{-# LANGUAGE InstanceSigs #-}
module Day09.Solution where

import Data.List.Split (splitOn, chunksOf)
import Data.List (lines, elemIndex, nub, sortOn, groupBy, sort, sortBy, intercalate, find, foldl', intercalate, transpose)
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Data.Bifunctor (first, second)
import Data.Ord (comparing, Down(Down))

type Point = (Int, Int)
type BiPoint = (Point, Point)
type Pair = (Point, Point)
type BiPair = (BiPoint, BiPoint)

data Color = Red | Green | Black deriving Eq

instance Show Color where
  show :: Color -> String
  show c =
    case c of
      Red -> "#"
      Green -> ";"
      Black -> "."

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

mseti :: [[a]] -> Int -> Int -> a -> [[a]]
mseti orig i j e =
  let
    (xr,yr:yrs) = splitAt i orig
    (xc,yc:ycs) = splitAt j yr
  in
    xr ++ [xc ++ e : ycs] ++ yrs

part1 :: String -> Int
part1 contents = maximum pairAreas where
  lns = lines contents

  pts = map (listToTuple . map read . splitOn ",") lns
  pairs = [(a, b) | a <- pts, b <- pts, a /= b]
  pairAreas = map getArea pairs

  getArea :: Pair -> Int
  getArea ((x1,y1),(x2,y2)) = (1 + abs (x2-x1)) * (1 + abs (y2-y1))

part2 :: String -> Int
part2 contents = maybe 0 snd firstValidPairArea where
  lns = lines contents

  points = map (listToTuple . map read . splitOn ",") lns :: [Point]
  pointsWithOriginalPos = zip points [0..] -- keeping track of original position to sort back to it later

  sortedPairsX = sortOn (fst . fst) pointsWithOriginalPos
  zippedSortedPairsX = zip sortedPairsX (map (`div` 2) [2..])
  sortedPairsY = sortOn (snd . fst . fst) zippedSortedPairsX
  zippedSortedPairsY = zip sortedPairsY (map (`div` 2) [2..])

  biPointsWithOriginalPos = map (\((((x1,y1),i),x2),y2) -> (((x1,y1),(x2,y2)), i)) zippedSortedPairsY :: [(BiPoint, Int)]
  biPoints = map fst $ sortOn snd biPointsWithOriginalPos -- sorted back to original position
  biPairs = [(a, b) | a <- biPoints, b <- biPoints, a /= b]

  maxIndex = length biPoints `div` 2
  baseMatrix = replicate (maxIndex + 2) $ replicate (maxIndex + 2) Black

  linedMatrix = fst $ foldl' depositColor (baseMatrix, head biPoints) $ tail biPoints
  linedWithRedMatrix = foldl (\m p@(_,(x,y)) -> mseti m x y Red) linedMatrix biPoints
  filledMatrix = fillStructure linedWithRedMatrix

  sortedPairAreas = sortOn (Down . snd) $ map (\p -> (p, getArea p)) biPairs
  firstValidPairArea = find (notElem Black . extractPerimeter filledMatrix . fst) sortedPairAreas

  depositColor :: ([[Color]], BiPoint) -> BiPoint -> ([[Color]], BiPoint)
  depositColor (mat, (_,(px,py))) pt@(_,(x,y))
    | px == x && py < y = (foldl' (\amat (lx,ly) -> mseti amat lx ly Green) mat [(x, ly) | ly <- [py..y]], pt)
    | px == x && py > y = (foldl' (\amat (lx,ly) -> mseti amat lx ly Green) mat [(x, ly) | ly <- [y..py]], pt)
    | py == y && px < x = (foldl' (\amat (lx,ly) -> mseti amat lx ly Green) mat [(lx, y) | lx <- [px..x]], pt)
    | py == y && px > x = (foldl' (\amat (lx,ly) -> mseti amat lx ly Green) mat [(lx, y) | lx <- [x..px]], pt)

  fillStructure :: [[Color]] -> [[Color]]
  fillStructure mat = map (map doColor) cXY where -- this only works because the structure is simple enough
    tmat = transpose mat
    maxIndex = length mat - 1
    indexMatrix = [[(x,y) | y <- [0..maxIndex]] | x <- [0..maxIndex]] 
    cX = zip mat indexMatrix
    cXY = map (uncurry zip) cX

    doColor :: (Color, (Int, Int)) -> Color
    doColor (c,(x,y)) =
      let
        rrow = drop (y + 1) $ mat !! x
        lrow = drop (maxIndex - y + 1) $ reverse $ mat !! x
        dcol = drop (x + 1) $ tmat !! y
        ucol = drop (maxIndex - x + 1) $ reverse $ tmat !! y
        walls = map (length . filter (not . null) . splitOn [Black]) [rrow, lrow, dcol, ucol]
      in
        if c == Black && notElem 0 walls
          then Green
          else c

  extractPerimeter :: [[Color]] -> BiPair -> [Color]
  extractPerimeter mat ((_,(x1,y1)),(_,(x2,y2))) = top ++ left ++ right ++ bottom where -- duplicate corners but whatever
    lx = min x1 x2
    hx = max x1 x2
    ly = min y1 y2
    hy = max y1 y2
    top    = [mat !! x !! hy | x <- [lx..hx]]
    left   = [mat !! lx !! y | y <- [ly..hy]]
    bottom = [mat !! x !! ly | x <- [lx..hx]]
    right  = [mat !! hx !! y | y <- [ly..hy]]

  getArea :: BiPair -> Int
  getArea (((x1,y1),_),((x2,y2),_)) = (1 + abs (x2-x1)) * (1 + abs (y2-y1))

part2alt :: String -> Int -- DOES NOT WORK on the sample input, but does on the full input.
part2alt contents = maybe 0 snd firstValidPairArea where
  lns = lines contents

  points = map (listToTuple . map read . splitOn ",") lns :: [Point]
  pointsWithOriginalPos = zip points [0..] -- keeping track of original position to sort back to it later

  sortedPairsX = sortOn (fst . fst) pointsWithOriginalPos
  zippedSortedPairsX = zip sortedPairsX (map (`div` 2) [2..])
  sortedPairsY = sortOn (snd . fst . fst) zippedSortedPairsX
  zippedSortedPairsY = zip sortedPairsY (map (`div` 2) [2..])

  biPointsWithOriginalPos = map (\((((x1,y1),i),x2),y2) -> (((x1,y1),(x2,y2)), i)) zippedSortedPairsY :: [(BiPoint, Int)]
  biPoints = map fst $ sortOn snd biPointsWithOriginalPos -- sorted back to original position
  biPairs = [(a, b) | a <- biPoints, b <- biPoints, a /= b]

  maxIndex = length biPoints `div` 2
  baseMatrix = replicate (maxIndex + 2) $ replicate (maxIndex + 2) True

  linedMatrix = fst $ foldl' depositColor (baseMatrix, head biPoints) $ tail biPoints
  -- linedWithRedMatrix = foldl (\m p@(_,(x,y)) -> mseti m x y Red) linedMatrix biPoints

  sortedPairAreas = sortOn (Down . snd) $ map (\p -> (p, getArea p)) biPairs
  firstValidPairArea = find (and . extractInnerPerimeter linedMatrix . fst) sortedPairAreas

  depositColor :: ([[Bool]], BiPoint) -> BiPoint -> ([[Bool]], BiPoint)
  depositColor (mat, (_,(px,py))) pt@(_,(x,y))
    | px == x && py < y = (foldl' (\amat (lx,ly) -> mseti amat lx ly False) mat [(x, ly) | ly <- [py..y]], pt)
    | px == x && py > y = (foldl' (\amat (lx,ly) -> mseti amat lx ly False) mat [(x, ly) | ly <- [y..py]], pt)
    | py == y && px < x = (foldl' (\amat (lx,ly) -> mseti amat lx ly False) mat [(lx, y) | lx <- [px..x]], pt)
    | py == y && px > x = (foldl' (\amat (lx,ly) -> mseti amat lx ly False) mat [(lx, y) | lx <- [x..px]], pt)

  extractInnerPerimeter :: [[Bool]] -> BiPair -> [Bool]
  extractInnerPerimeter mat ((_,(x1,y1)),(_,(x2,y2))) = top ++ left ++ right ++ bottom where -- duplicate corners but whatever
    tmat = transpose mat
    lx = min x1 x2 + 1
    hx = max x1 x2 - 1
    ly = min y1 y2 + 1
    hy = max y1 y2 - 1
    top    = take (hy - ly + 1) $ drop ly $ mat !! hx
    left   = take (hx - lx + 1) $ drop lx $ tmat !! ly
    bottom = take (hy - ly + 1) $ drop ly $ mat !! lx
    right  = take (hx - lx + 1) $ drop lx $ tmat !! hy

  getArea :: BiPair -> Int
  getArea (((x1,y1),_),((x2,y2),_)) = (1 + abs (x2-x1)) * (1 + abs (y2-y1))

main :: IO ()
main = do
  sample <- readFile "src/Day09/sample.txt"
  input <- readFile "src/Day09/input.txt"

  print $ part1 sample
  print $ part2 sample

  print $ part1 input
  print $ part2 input

  return ()

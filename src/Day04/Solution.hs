module Day04.Solution where

import Data.List (lines, elemIndex, isPrefixOf, find)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Bifunctor (first, second)

mseti :: [[a]] -> Int -> Int -> a -> [[a]]
mseti orig i j e =
  let
    (xr,yr:yrs) = splitAt i orig
    (xc,yc:ycs) = splitAt j yr
  in
    xr ++ [xc ++ e : ycs] ++ yrs

part1 :: String -> Int
part1 contents = length $ concatMap (filter isMovablePaper . findLinePaper) contentLines where
  contentLines = lines contents
  numLines = length contentLines
  lineLen = length $ head contentLines

  findLinePaper :: String -> [(Int, Int)] -- i, j
  findLinePaper [] = []
  findLinePaper line = flph line 0 where -- helper used to keep full line in scope
    flph [] _ = []
    flph lineSeg@(x:xs) j =
      if x == '@'
      then (fromJust $ elemIndex line contentLines, j) : flph xs (j+1)
      else flph xs (j+1)
  
  isMovablePaper :: (Int, Int) -> Bool
  isMovablePaper (i, j) = adjPaper < 4 where
      adjPaper = length [ (a, b)
                        | a <- [(i-1)..(i+1)]
                        , b <- [(j-1)..(j+1)]
                        , not (a == i && b == j) &&
                          a >= 0 &&
                          b >= 0 &&
                          a < numLines
                          && b < lineLen
                          && contentLines !! a !! b == '@' ]

part2 :: String -> Int
part2 contents = length $ concat $ iterateMap $ lines contents where
  
  iterateMap :: [String] -> [[(Int, Int)]]
  iterateMap map = let 
                     movable = concatMap (filter isMovablePaper . findLinePaper) map
                   in
                    if not (null movable)
                      then movable : iterateMap (updateMap map movable) 
                      else [] 
                   where
    numLines = length map
    lineLen = length $ head map

    findLinePaper :: String -> [(Int, Int)] -- i, j
    findLinePaper [] = []
    findLinePaper line = flph line 0 where -- helper used to keep full line in scope
      flph [] _ = []
      flph lineSeg@(x:xs) j =
        if x == '@'
        then (fromJust $ elemIndex line map, j) : flph xs (j+1)
        else flph xs (j+1)
    
    isMovablePaper :: (Int, Int) -> Bool
    isMovablePaper (i, j) = adjPaper < 4 where
      adjPaper = length [ (a, b)
                        | a <- [(i-1)..(i+1)]
                        , b <- [(j-1)..(j+1)]
                        , not (a == i && b == j) &&
                          a >= 0 &&
                          b >= 0 &&
                          a < numLines
                          && b < lineLen
                          && map !! a !! b == '@' ]

    updateMap :: [String] -> [(Int, Int)] -> [String]
    updateMap old [] = old
    updateMap old ((i,j):xs) = updateMap (mseti old i j '.') xs

main :: IO ()
main = do
  contents <- readFile "src/Day04/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

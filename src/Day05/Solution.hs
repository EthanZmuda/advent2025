module Day05.Solution where

import Data.List.Split (splitOn)
import Data.List (lines, elemIndex, isPrefixOf, find, permutations)
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Data.Bifunctor (first, second)

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

part1 :: String -> Int
part1 contents = length $ filter isFresh iids where
  [rawRanges, rawIids] = splitOn [""] $ lines contents
  iids = map read rawIids
  ranges = map (listToTuple . map read . splitOn "-") rawRanges

  isFresh :: Int -> Bool
  isFresh iid = any (checkFresh iid) ranges

  checkFresh :: Int -> (Int, Int) -> Bool
  checkFresh iid (lower, upper) = iid >= lower && iid <= upper

part2 :: String -> Int
part2 contents = sum $ map (\(x,y) -> y - x + 1) consolidatedRanges where
  (rawRanges:_) = splitOn [""] $ lines contents
  ranges = map (listToTuple . map read . splitOn "-") rawRanges
  consolidatedRanges = foldl processRange [] ranges

  processRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
  processRange acc range = acc ++ catMaybes (foldl pareAccs [Just range] acc) where

    pareAllRanges :: [Maybe (Int, Int)] -> (Int, Int) -> [Maybe (Int, Int)]
    pareAllRanges paredAccs (rl, ru) = []

    pareAccs :: [Maybe (Int, Int)] -> (Int, Int) -> [Maybe (Int, Int)]
    pareAccs paredAccs thisRange = concatMap (pareAcc thisRange) paredAccs

    pareAcc :: (Int, Int) -> Maybe (Int, Int) -> [Maybe (Int, Int)]
    pareAcc (rl,ru) paredAcc =
      case paredAcc of
        Nothing -> [Nothing]
        Just (al,au)
          | al > ru || au < rl -> [Just (al, au)] -- doesn't pare
          | al < rl && au > ru -> [Just (al, rl-1), Just (ru+1,au)] -- pares middle
          | al < rl && au >= rl -> [Just (al, rl-1)] -- pares lower
          | al <= ru && au > ru -> [Just (ru+1, au)] -- pares upper
          | otherwise -> [Nothing] -- wiped

main :: IO ()
main = do
  contents <- readFile "src/Day05/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

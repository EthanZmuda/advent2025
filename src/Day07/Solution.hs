module Day07.Solution where

import Data.List.Split (splitOn, chunksOf)
import Data.List (lines, elemIndex, nub, sortOn, groupBy)
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Data.Bifunctor (first, second)

part1 :: String -> Int
part1 contents = snd finalBeams where
  lns = lines contents
  firstBeam = fromJust $ elemIndex 'S' $ head lns
  finalBeams = foldl calculateBeam ([firstBeam], 0) $ tail lns

  calculateBeam :: ([Int], Int) -> String -> ([Int], Int)
  calculateBeam (beams, splits) ln = (nub $ concat newBeams, splits + length (filter (\x -> length x == 2) newBeams)) where
    newBeams = map checkBeam beams

    checkBeam :: Int -> [Int]
    checkBeam i
      | ln !! i == '^' = [i-1, i+1]
      | otherwise      = [i]

part2 :: String -> Int
part2 contents = sum $ map snd finalBeams where
  lns = lines contents
  firstBeam = fromJust $ elemIndex 'S' $ head lns
  finalBeams = foldl calculateBeam [(firstBeam, 1)] $ tail lns

  calculateBeam :: [(Int, Int)] -> String -> [(Int, Int)]
  calculateBeam beams ln = combinedBeams where
    newBeams = concatMap checkBeam beams
    combinedBeams = map combineGroup $ groupBy (\x y -> fst x == fst y) $ sortOn fst newBeams

    combineGroup :: [(Int, Int)] -> (Int, Int)
    combineGroup g = (fst $ head g, sum $ map snd g)

    checkBeam :: (Int, Int) -> [(Int, Int)]
    checkBeam (i,k)
      | ln !! i == '^' = [(i-1,k), (i+1,k)]
      | otherwise      = [(i,k)]

main :: IO ()
main = do
  contents <- readFile "src/Day07/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

module Day06.Solution where

import Data.List.Split (splitOn, chunksOf)
import Data.List (lines, elemIndex, isPrefixOf, find, transpose)
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Data.Bifunctor (first, second)

part1 :: String -> Int
part1 contents = sum $ map calculateProblem problems where
  lns = lines contents
  delimitedLns = map (filter (not . null) . splitOn " ") lns
  rawProblems = transpose delimitedLns
  problems = map formatProblem rawProblems

  formatProblem :: [String] -> ([Int], Char)
  formatProblem strs = let ([last]:revNums) = reverse strs in (map read $ reverse revNums, last)

  calculateProblem :: ([Int], Char) -> Int
  calculateProblem (nums, '+') = sum nums
  calculateProblem (nums, '*') = product nums

part2 :: String -> Int
part2 contents = sum $ map calculateProblem problems where
  lns = lines contents
  flipped = transpose lns
  groupedByProblem = reverse $ map reverse $ foldl grouph [[]] flipped
  rawProblems = map extractSymbol groupedByProblem
  problems = map (first $ map read) rawProblems

  grouph :: [[String]] -> String -> [[String]]
  grouph acc@(x:xs) str
    | all (==' ') str = []:acc
    | otherwise       = (str:x):xs

  extractSymbol :: [String] -> ([String], Char)
  extractSymbol strs = let symbol = last $ head strs in (map (reverse . drop 1 . reverse) strs, symbol)

  calculateProblem :: ([Int], Char) -> Int
  calculateProblem (nums, '+') = sum nums
  calculateProblem (nums, '*') = product nums

main :: IO ()
main = do
  contents <- readFile "src/Day06/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

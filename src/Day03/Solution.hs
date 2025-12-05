module Day03.Solution where

import Data.Char (digitToInt)
import Data.Bifunctor (first, second)

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

part1 :: String -> Int
part1 contents = sum $ map findJoltage $ lines contents where

  findJoltage :: String -> Int
  findJoltage line = tensNum * 10 + onesNum where
    ll = length line
    (tensNum, tensPos) = findHighest $ dropEnd 1 line
    onesNum = fst (findHighest $ drop (tensPos + 1) line)

    findHighest :: String -> (Int, Int)
    findHighest numStr = let
      res = foldl cmpFunc ((0, 0), 0) numStr
      in fst res
    
    cmpFunc :: ((Int, Int), Int) -> Char -> ((Int, Int), Int)
    cmpFunc acc d
      | digitToInt d > (fst . fst) acc = ((digitToInt d, snd acc), snd acc + 1)
      | otherwise                      = second (+1) acc

part2 :: String -> Int
part2 contents = sum (map findJoltage $ lines contents) where

  findJoltage :: String -> Int
  findJoltage line = read $ foldl (\acc d -> acc ++ show (fst d)) "" $ grabDigits 12 where

    grabDigits :: Int -> [(Int, Int)]
    grabDigits = grabDigitsHelper (-1) []

    grabDigitsHelper :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
    grabDigitsHelper lastIndex npList step =
      case step of
        0 -> npList
        x -> let
               np = findHighest (lastIndex + 1) (drop (lastIndex + 1) $ dropEnd (step - 1) line) 
             in
               grabDigitsHelper (snd np) (npList ++ [np]) (step - 1)

    findHighest :: Int -> String -> (Int, Int)
    findHighest startIndex numStr =
      let
        res = foldl cmpFunc ((0, 0), startIndex) numStr
      in
        fst res
    
    cmpFunc :: ((Int, Int), Int) -> Char -> ((Int, Int), Int)
    cmpFunc acc d
      | digitToInt d > (fst . fst) acc = ((digitToInt d, snd acc), snd acc + 1)
      | otherwise                      = second (+1) acc

main :: IO ()
main = do
  contents <- readFile "src/Day03/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

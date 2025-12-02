module Main where

import System.IO.Unsafe (unsafePerformIO)

part1 :: String -> Int
part1 contents = snd ans where
  ans = foldl accFunc (50, 0) steps
  steps = lines contents
  accFunc acc step =
    case step of
      'L':xs -> let total = (fst acc - read xs) `mod` 100 in (total, snd acc + fromEnum (total == 0))
      'R':xs -> let total = (fst acc + read xs) `mod` 100 in (total, snd acc + fromEnum (total == 0))

part2 :: String -> Int
part2 contents = snd ans where
  ans = foldl accFunc (50, 0) steps
  steps = lines contents

  accFunc :: (Int, Int) -> String -> (Int, Int)
  accFunc acc step =
    case step of
      'L':xs -> let total = fst acc - read xs in (total `mod` 100, snd acc + handleIncrementL total (fst acc))
      'R':xs -> let total = fst acc + read xs in (total `mod` 100, snd acc + (total `div` 100))
  
  handleIncrementL :: Int -> Int -> Int
  handleIncrementL t prev
    | t <= 0    = fromEnum (prev /= 0) + ((-t) `div` 100)
    | otherwise = 0

main :: IO ()
main = do
  contents <- readFile "src/01/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

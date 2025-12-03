module Day02.Solution where

import Data.List.Split (splitOn, chunksOf)

type TextInput = String

factorize :: Int -> [Int]
factorize num = foldl addIfFactor [] [1..(num `div` 2)] where
  
  addIfFactor :: [Int] -> Int -> [Int]
  addIfFactor acc new
    | num `mod` new == 0 = new:acc
    | otherwise = acc

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:rest) = x == y && allEqual (y:rest)

part1 :: TextInput -> Int
part1 contents = foldl nextStep 0 (splitOn "," contents) where

  nextStep :: Int -> String -> Int
  nextStep acc step = let [x,y] = splitOn "-" step in
    foldl addIfInvalid acc [(read x)..(read y)]

  addIfInvalid :: Int -> Int -> Int
  addIfInvalid acc num
    | isInvalid num = acc + num
    | otherwise     = acc
  
  isInvalid :: Int -> Bool
  isInvalid num
    | even $ length (show num) = take (length (show num) `div` 2) (show num) == drop (length (show num) `div` 2) (show num)
    | otherwise = False

part2 :: TextInput -> Int
part2 contents = foldl nextStep 0 (splitOn "," contents) where

  nextStep :: Int -> String -> Int
  nextStep acc step = let [x,y] = splitOn "-" step in
    foldl addIfInvalid acc [(read x)..(read y)]

  addIfInvalid :: Int -> Int -> Int
  addIfInvalid acc num
    | isInvalid num = acc + num
    | otherwise     = acc
  
  isInvalid :: Int -> Bool
  isInvalid num = any (\x -> allEqual . chunksOf x $ show num) (factorize . length . show $ num)

main :: IO ()
main = do
  contents <- readFile "src/Day02/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

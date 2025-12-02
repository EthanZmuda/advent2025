module Main where

import System.IO.Unsafe (unsafePerformIO)

type TextInput = String
type Instruction = String
type Dial = Int
type SeenZeros = Int

part1 :: TextInput -> SeenZeros
part1 contents = snd (foldl accFunc (50, 0) $ lines contents) where

  accFunc :: (Dial, SeenZeros) -> Instruction -> (Dial, SeenZeros)
  accFunc acc ('L':xs) = combine ((fst acc - read xs) `mod` 100) (snd acc)
  accFunc acc ('R':xs) = combine ((fst acc + read xs) `mod` 100) (snd acc)

  combine :: Dial -> SeenZeros -> (Dial, SeenZeros)
  combine total inc = (total, inc + fromEnum (total == 0))

part2 :: TextInput -> SeenZeros
part2 contents = snd (foldl accFunc (50, 0) $ lines contents) where

  accFunc :: (Dial, SeenZeros) -> Instruction -> (Dial, SeenZeros)
  accFunc acc step =
    case step of
      'L':xs -> let total = fst acc - read xs in (total `mod` 100, snd acc + handleIncrementL total (fst acc))
      'R':xs -> let total = fst acc + read xs in (total `mod` 100, snd acc + (total `div` 100))
  
  handleIncrementL :: Dial -> Dial -> SeenZeros
  handleIncrementL t prev
    | t <= 0    = fromEnum (prev /= 0) + ((-t) `div` 100)
    | otherwise = 0

main :: IO ()
main = do
  contents <- readFile "01/input.txt"
  print $ part1 contents
  print $ part2 contents

  return ()

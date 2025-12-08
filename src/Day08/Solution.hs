module Day08.Solution where

import Data.List.Split (splitOn, chunksOf)
import Data.List (lines, elemIndex, nub, sortOn, groupBy, sort, sortBy)
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Data.Bifunctor (first, second)
import Data.Ord (comparing, Down(Down))
import Data.Tuple.Extra (fst3)

type Point = (Int, Int, Int)
type Pair = (Point, Point)
type Network = [Point] -- Not all Point arrays are Networks, so not used everywhere

listTo3ple :: [a] -> (a,a,a)
listTo3ple [x,y,z] = (x,y,z)

takeEveryOther :: [a] -> [a]
takeEveryOther [] = []
takeEveryOther (x1:x2:xs) = x1 : takeEveryOther xs

part1 :: String -> Int -> Int
part1 contents steps = product $ take 3 $ sortBy (comparing Data.Ord.Down) $ map length networks where
  lns = lines contents

  pts = map (listTo3ple . map read . splitOn ",") lns
  pairs = [(x, y) | x <- pts, y <- pts, x /= y]
  pairDistances = map (\p -> (p, getDistance p)) pairs
  sortedPairs = map fst $ takeEveryOther $ sortOn snd pairDistances

  networks = makeConnections sortedPairs (map (:[]) pts) steps

  getDistance :: Pair -> Float
  getDistance pair@((x1,y1,z1),(x2,y2,z2)) = sqrt $ fromIntegral ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1))

  makeConnections :: [Pair] -> [Network] -> Int -> [Network]
  makeConnections _ net 0 = net
  makeConnections (p:ps) net step = makeConnections ps (joinNetworks net p ([],[])) (step-1)
    
  joinNetworks :: [Network] -> Pair -> (Network, Network) -> [Network]
  joinNetworks [] _ (l,r) = [l ++ r]
  joinNetworks (n:ns) (p1,p2) (l,r)
    | p1 `elem` n = joinNetworks ns (p1,p2) (n,r)
    | p2 `elem` n = joinNetworks ns (p1,p2) (l,n)
    | otherwise   = n : joinNetworks ns (p1,p2) (l,r)

part2 :: String -> Int
part2 contents = fst3 (fst lastConnection) * fst3 (snd lastConnection) where
  lns = lines contents

  pts = map (listTo3ple . map read . splitOn ",") lns
  pairs = [(x, y) | x <- pts, y <- pts, x /= y]
  pairDistances = map (\p -> (p, getDistance p)) pairs
  sortedPairs = map fst $ takeEveryOther $ sortOn snd pairDistances

  lastConnection = makeConnectionsUntilConnected sortedPairs (map (:[]) pts)

  getDistance :: Pair -> Float
  getDistance pair@((x1,y1,z1),(x2,y2,z2)) = sqrt $ fromIntegral ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1))

  makeConnectionsUntilConnected :: [Pair] -> [Network] -> Pair
  makeConnectionsUntilConnected (p1:p2:ps) net
    | length net == 1 = p1
    | otherwise       = makeConnectionsUntilConnected (p2:ps) (joinNetworks net p2 ([],[]))
    
  joinNetworks :: [Network] -> Pair -> (Network, Network) -> [Network]
  joinNetworks [] _ (l,r) = [l ++ r]
  joinNetworks (n:ns) (p1,p2) (l,r)
    | p1 `elem` n = joinNetworks ns (p1,p2) (n,r)
    | p2 `elem` n = joinNetworks ns (p1,p2) (l,n)
    | otherwise   = n : joinNetworks ns (p1,p2) (l,r)

main :: IO ()
main = do
  sample <- readFile "src/Day08/sample.txt"
  input <- readFile "src/Day08/input.txt"

  print $ part1 sample 10
  print $ part2 sample

  print $ part1 input 1000
  print $ part2 input

  return ()

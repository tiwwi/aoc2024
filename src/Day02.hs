module Day02 (solve) where

import Helpers.List (count, pairwise)

type Report = [Int]
type Input = [Report]

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- readFile fname
  let input = parseInput txt
  return (show $ part1 input, show $ part2 input)

parseInput :: String -> Input
parseInput = map (map read . words) . lines

isSafe, isSafe2 :: Report -> Bool
isSafe report = (all (> 0) diffs || all (< 0) diffs) && all (bounded . abs) diffs
  where
    diffs = [y - x | (x, y) <- pairwise report]
    bounded x = 1 <= x && x <= 3
isSafe2 = any isSafe . allOthers

allOthers :: Report -> [Report]
allOthers [] = []
allOthers (x : xs) = xs : ((x :) <$> allOthers xs)
-- Alternatively:
-- allOthers report = zipWith (<>) (inits report) (tail $ tails report)

part1 :: Input -> Int
part1 = count isSafe

part2 :: Input -> Int
part2 = count isSafe2

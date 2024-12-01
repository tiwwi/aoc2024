{-# LANGUAGE TupleSections #-}
module Day01 (solve) where

import Data.Bifunctor (bimap)
import Data.List (sort, transpose)
import Data.IntMap.Strict qualified as M
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)

type List = [Int]
type Input = (List, List)

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- readFile fname
  let input = parseInput txt
  return (show $ part1 input, show $ part2 input)

parseInput :: String -> Input
parseInput = tuplify . map (map read) . transpose . map words . lines
  where
    tuplify [x, y] = (x, y)
    tuplify _ = error "invalid input"

part1 :: Input -> Int
part1 = sum . uncurry (zipWith diff) . bimap sort sort
  where
    diff a b = abs $ a - b

part2 :: Input -> Int
part2 (list1, list2) = sum $ entryScore <$> list2
  where list1C = M.fromListWith (+) $ (,1) <$> list1
        getCount = fromMaybe 0 . (list1C M.!?)
        entryScore = liftA2 (*) getCount id

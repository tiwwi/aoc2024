module Helpers.List (
    takeUntil,
    allPairs,
    pairwise,
    count,
    countUnique,
    dedup,
    takeWhileJust,
    counter,
) where

import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
    | f x = [x]
    | otherwise = x:takeUntil f xs

allPairs :: [a] -> [(a, a)]
allPairs xs = [ (x,y) | (x:ys) <- tails xs, y <- ys ]

pairwise :: [a] -> [(a, a)]
pairwise (x:xs'@(y:_)) = (x,y):pairwise xs'
pairwise _ = []

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

countUnique :: Ord a => [a] -> Int
countUnique = S.size . S.fromList

dedup :: Ord a => [a] -> [a]
dedup = S.toList . S.fromList

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = foldr (maybe (const []) (:)) []

counter :: Ord a => [a] -> M.Map a Int
counter xs = M.fromListWith (+) $ (,1) <$> xs


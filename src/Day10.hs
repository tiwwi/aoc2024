module Day10 (solve) where

import Helpers.Matrix
import Data.Char (digitToInt)
import Helpers.List

type Board = UArray Pos Int
solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- readFile fname
    let input = amap digitToInt (readAOCMatrix txt)
    return (show $ part1 input, show $ part2 input)

hikeDFS :: Board -> Array Pos [Pos]
hikeDFS arr = dfsArr
    where dfsArr = listArray (bounds arr) (go <$> assocs arr)
          go (pos, 9) = [pos]
          go (pos, val) = dedup $ (concatMap ((dfsArr !) . fst) $ filter ((== val + 1) . snd) $ cardinalNbs arr pos)

hikePaths :: Board -> Array Pos Int
hikePaths arr = dfsArr
    where dfsArr = listArray (bounds arr) (go <$> assocs arr)
          go (_, 9) = 1
          go (pos, val) = sum $ (dfsArr !) . fst <$> (filter ((== val + 1) . snd) $ cardinalNbs arr pos)

part1, part2 :: Board -> Int
part1 arr = sum [length l | (p, l) <- assocs $ hikeDFS arr, arr ! p == 0]
part2 arr = sum [l | (p, l) <- assocs $ hikePaths arr, arr ! p == 0]


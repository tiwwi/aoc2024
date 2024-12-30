{-# LANGUAGE OverloadedStrings #-}
module Day18 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text
import Helpers.Matrix
import Helpers.Algorithms
import Data.Set qualified as S
import Data.Map qualified as M
import Data.Ix
import Debug.Trace
import Data.Vector qualified as V
import Data.Maybe (isNothing)

type Blocks = V.Vector (S.Set Pos)

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseInput txt
    return (show $ part1 input 1024, show $ part2 input)

parseInput :: T.Text -> Blocks
parseInput input = V.fromList blockList
    where blockList = scanl (flip S.insert) S.empty $ map parseLine $ T.lines input
          parseLine line = case T.splitOn "," line of
                            [x,y] -> V2 (readT x) (readT y)

binS :: (Int, Int) -> (Int -> Bool) -> Maybe (Int, Int)
binS (lo, hi) p  = case (pm, pm') of
                    (False, True) -> Just (m, m')
                    (False, False) -> binS (m',hi) p
                    (True, True) -> binS (lo, m) p
                    (True, False) -> Nothing
    where m = (lo + hi) `div` 2
          m' = m+1
          pm = p m
          pm' = p m'

part1 :: Blocks -> Int -> Maybe Int
part1 blocks n = fst <$> (fst $ dijkstra start nbf isGoal)
    where dropped = blocks V.! n
          start = V2 0 0
          end = V2 70 70
          bounds = (start, end)
          isGoal = (== end)
          nbf pos = [ (1,nPos) | d <- cardinals, let nPos = pos + d, inRange bounds nPos && nPos `S.notMember` dropped]

part2 :: Blocks -> S.Set Pos
part2 blocks = S.difference (blocks V.! hi) (blocks V.! lo)
    where pred i = isNothing $ part1 blocks i
          Just (lo,hi) = binS (0, V.length blocks - 1) pred 

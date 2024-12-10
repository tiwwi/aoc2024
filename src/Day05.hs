{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day05 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Attoparsec.Text qualified as A
import Data.Graph
import Helpers.List
import Helpers.Text
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Order = Order Int Int deriving (Show, Eq, Ord)
type Manual = S.Set Order
type Input = (Manual, [Update])
type Update = [Int]

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let (orders, updates) = parseInput txt
    return (show $ part1 orders updates, show $ part2 orders updates)

inputP :: A.Parser Input
inputP = (,) <$> (S.fromList <$> A.many1 orderP) <* A.skipSpace <*> A.many1 updateP
  where orderP = Order <$> (A.decimal) <*> (A.char '|' *> A.decimal) <* A.endOfLine
        updateP = (A.decimal `A.sepBy1` A.char ',') <* A.endOfLine

isValid :: Manual -> Update -> Bool
isValid allowed update = all good $ allPairs update
    where good (a,b) = Order b a `S.notMember` allowed

reorder :: M.Map Int (S.Set Int) -> Update -> Update
reorder manual upd =  (\(k, _, _) -> k) . vertexMap <$> topSort graph
    where updS = S.fromList upd
          partManual = S.intersection updS <$> M.restrictKeys manual updS
          partEdges = [(k, k, S.elems st) | (k, st) <- M.assocs partManual]
          (graph, vertexMap, _) = graphFromEdges partEdges

parseInput :: T.Text -> Input
parseInput = quickParseT inputP

middle :: Update -> Int
middle upd = upd !! (length upd `div` 2)

part1 :: Manual -> [Update] -> Int
part1 manual = sum . map middle . filter (isValid manual)
part2 :: Manual -> [Update] -> Int
part2 manual = sum . map (middle . reorder manGraph) . filter (not . isValid manual)
    where manGraph = M.fromListWith (S.union) [(a, S.singleton b) | Order a b <- S.toList manual]

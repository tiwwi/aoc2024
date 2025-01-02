{-# LANGUAGE OverloadedStrings #-}
module Day23 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Helpers.List
import Data.List (maximumBy)
import Data.Function (on)

type Node = T.Text
type Edge = (T.Text, T.Text)
type Graph = M.Map Node (S.Set Node)

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let edges = parseInput <$> T.lines txt
        graph = toGraph edges
    return (show $ part1 graph, show $ part2 graph)

parseInput = tuplify . T.splitOn "-"
    where tuplify [x,y] = (x,y)


toGraph :: [Edge] -> Graph
toGraph edges = foldr go M.empty edges
    where go (v1, v2) mp = M.unionWith S.union mp $ M.fromList [(v1, S.singleton v2), (v2, S.singleton v1)]

find3Cliques :: Graph -> [S.Set Node]
find3Cliques graph = dedup $ M.assocs graph >>= cliquesAt
    where cliquesAt (v, vnbs) = do
            w <- S.toList vnbs
            let wnbs = graph M.! w
                trips = S.intersection vnbs wnbs
            u <- S.toList trips
            pure $ S.fromList [u,v,w]

largest :: [S.Set Node] -> S.Set Node
largest = maximumBy (compare `on` S.size)

maximumCliqueIn :: Graph -> S.Set Node -> S.Set Node -> S.Set Node
maximumCliqueIn graph potential found = maxClique
    where (maxClique, _) = foldl go (found, potential) $ S.toList potential
          go (clique, potential') node = (largest [foundClique, clique], S.difference potential' foundClique)
            where foundClique = maximumCliqueIn graph (S.intersection (graph M.! node) potential') (S.insert node found)
            

part1 = count withT . find3Cliques . traceShowWith (M.size)
    where withT s = any (T.isPrefixOf "t") s
part2 graph = T.intercalate "," $ S.toList $ maxClique
    where maxClique = maximumCliqueIn graph (M.keysSet graph) S.empty
    

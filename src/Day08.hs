module Day08 (solve) where

import Helpers.Matrix
import qualified Data.Map as M
import qualified Data.Set as S
import Helpers.List

data Roof = Roof {bds::(Pos, Pos), antennas::M.Map Char [Pos]}

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- readFile fname
    let input = parseInput txt
    return (show $ part1 input, show $ part2 input)

parseInput str = Roof (bounds mat) out
    where mat = readAOCMatrix str
          out = M.fromListWith (++) [(c, [pos]) | (pos, c) <- assocs mat, c /= '.']

findAntinodes :: (Roof -> (Pos, Pos) -> [Pos]) -> Roof -> S.Set Pos
findAntinodes f roof = foldMap go $ M.elems (antennas roof)
    where go positions = S.fromList $ allPairs positions >>= (f roof)

antinodes1 (Roof bds _) (a,b) = filter (inRange bds) [(2*^a) - b, (2*^b) - a]
antinodes2 (Roof bds _) (a,b) = takeWhile (inRange bds) [a + k*^(a-b) | k <- [0..]] ++
                            takeWhile (inRange bds) [b + k*^(b-a) | k <- [0..]] 

parts :: (Roof -> (Pos, Pos) -> [Pos]) -> Roof -> Int
parts f = S.size . findAntinodes f

part1, part2 :: Roof -> Int
part1 = parts antinodes1
part2 = parts antinodes2

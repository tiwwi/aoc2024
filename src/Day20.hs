module Day20 (solve) where

import Helpers.Matrix
import Helpers.Algorithms
import Data.Map qualified as M
import Helpers.List (count)

type Maze = Array Pos Field
data Race = Race {start :: Pos, end :: Pos, maze :: Array Pos Field}
data Field = Wall | Empty deriving (Eq)

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- readFile fname
    let input = parseInput txt
    return (show $ part input 2, show $ part input 20)

fromChar :: Char -> Field
fromChar '#' = Wall
fromChar _ = Empty

parseInput :: String -> Race
parseInput txt = Race start end (fromChar <$> mat)
  where
    mat = readAOCMatrix txt
    Just start = findArrayIndex (== 'S') mat
    Just end = findArrayIndex (== 'E') mat

nbs :: Maze -> Pos -> [Pos]
nbs maze pos = [pos + d | d <- cardinals, maze ! (pos + d) == Empty] 

cheatsFrom :: Maze -> Int -> Pos -> [(Pos,Pos)]
cheatsFrom maze n pos@(V2 px py) = map (pos,) $ filter (\pos -> maze !? pos == Just Empty) $ cheatStep 
    where cheatStep = [V2 (px + i) (py + j) | i <- [-n..n], let jbound = n - (abs i), j <- [-jbound .. jbound]]

part :: Race -> Int -> Int
part (Race start _ maze) n = count (>= 100) $ (timeSaved <$> (positions >>= cheatsFrom maze n))
    where bfsMap = bfs start (nbs maze)
          timeSaved (start, end) = (bfsMap M.! end - bfsMap M.! start) - (sum $ abs <$> end - start)
          positions = M.keys bfsMap
part2 :: Race -> String
part2 = const "unfinished"

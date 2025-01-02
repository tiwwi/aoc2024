module Day16 (solve) where

import Helpers.Algorithms (dijkstra)
import Helpers.Matrix
import Data.Map qualified as M
import Helpers.List (dedup)

data Field = Wall | Empty deriving (Eq)

data Race = Race {start :: Pos, end :: Pos, maze :: Array Pos Field}
data Deer = Deer {pos :: Pos, dir :: Dir} deriving (Eq, Ord, Show)

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- readFile fname
  let input = parseInput txt 
      (p1sol, p1map) = part1 input
  return (show $ p1sol, show $ part2 input p1map p1sol)

instance Show Field where
  show Wall = "#"
  show Empty = "."

fromChar :: Char -> Field
fromChar '#' = Wall
fromChar _ = Empty

parseInput :: String -> Race
parseInput txt = Race start end (fromChar <$> mat)
  where
    mat = readAOCMatrix txt
    Just start = findArrayIndex (== 'S') mat
    Just end = findArrayIndex (== 'E') mat


part1 (Race start end maze) = (endv, results)
  where
    endv = minimum $ [ results M.! d | d <- Deer end <$> cardinals]
    deerNbs (Deer pos dir) =
        ( if maze ! (pos + dir) == Empty
            then [(1, Deer (pos + dir) dir)]
            else []
        )
          ++ [ (1000, Deer pos (rotateRight dir)),
               (1000, Deer pos (rotateLeft dir))
             ]
    initialDeer = Deer start right
    deerGoal = const False
    (Nothing, results) = dijkstra initialDeer deerNbs deerGoal

part2 (Race start end maze) p1results p1val = length $ dedup $ [pos deer | (deer, dist) <- M.assocs endv, dist == p1val]
    where endv = M.unionWith (+) p1results $ (foldr1 (M.unionWith min) results)
          deerNbs (Deer pos dir) =
              ( if maze ! (pos - dir) == Empty
                  then [(1, Deer (pos - dir) dir)]
                  else []
              )
              ++ [ (1000, Deer pos (rotateRight dir)),
                  (1000, Deer pos (rotateLeft dir))
                  ]
          initialDeers = Deer end <$> [right, up]
          deerGoal = const False
          results = map (\d -> snd $ dijkstra d deerNbs deerGoal) initialDeers
  

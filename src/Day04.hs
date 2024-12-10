module Day04 (solve) where

import Data.Array.IArray
import Data.List (isPrefixOf)
import Helpers.Matrix (AOCMatrix, readAOCMatrix)
import Helpers.List (count)
import Linear.V2
import Linear.Vector ((*^))

type Input = AOCMatrix

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- readFile fname
  let input = parseInput txt
  return (show $ part1 input, show $ part2 input)

parseInput :: String -> AOCMatrix
parseInput = readAOCMatrix

part1, part2 :: Input -> Int
part1 board = sum $ xmasCount <$> assocs board
  where
    dirs = tail $ V2 <$> [0, -1, 1] <*> [0, -1, 1]
    dirString start dir = [board !? (start + (i *^ dir)) | i <- [1 ..]]
    xmasCount (pos, 'X') = count (map Just "MAS" `isPrefixOf`) $ dirString pos <$> dirs
    xmasCount (_, _) = 0

part2 board = count valid $ assocs board
  where valid (pos, 'A') =  and $ (isMas . map ((board !?) . (+pos))) <$> masks
        valid (_, _) = False
        masks = [[ sign *^ pos | sign <- [-1, 1]] | pos <- [V2 1 1, V2 1 (-1)]]
        isMas = (`elem` (map (map Just) ["MS", "SM"]))

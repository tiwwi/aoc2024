module Day22 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T

type Input = [T.Text]

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseInput txt
    return (show $ part1 input, show $ part2 input)

parseInput = undefined

part1 :: Input -> String
part1 = const "unfinished"
part2 :: Input -> String
part2 = const "unfinished"

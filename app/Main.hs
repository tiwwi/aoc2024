module Main where

import Control.Monad ((>=>))
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Time.Clock
import Control.DeepSeq (deepseq)

import AOCFiles

import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import qualified Day08 (solve)
import qualified Day09 (solve)
import qualified Day10 (solve)
import qualified Day11 (solve)
import qualified Day12 (solve)
import qualified Day13 (solve)
import qualified Day14 (solve)
import qualified Day15 (solve)
import qualified Day16 (solve)
import qualified Day17 (solve)
import qualified Day18 (solve)
import qualified Day19 (solve)
import qualified Day20 (solve)
import qualified Day21 (solve)
import qualified Day22 (solve)
import qualified Day23 (solve)
import qualified Day24 (solve)
import qualified Day25 (solve)

inputFile :: Int -> FilePath
inputFile n = "inputs/day" ++ printf "%02d" n ++ ".in"

solveDay :: Int -> IO (String, String)
solveDay 1 = Day01.solve $ inputFile 1
solveDay 2 = Day02.solve $ inputFile 2
solveDay 3 = Day03.solve $ inputFile 3
solveDay 4 = Day04.solve $ inputFile 4
solveDay 5 = Day05.solve $ inputFile 5
solveDay 6 = Day06.solve $ inputFile 6
solveDay 7 = Day07.solve $ inputFile 7
solveDay 8 = Day08.solve $ inputFile 8
solveDay 9 = Day09.solve $ inputFile 9
solveDay 10 = Day10.solve $ inputFile 10
solveDay 11 = Day11.solve $ inputFile 11
solveDay 12 = Day12.solve $ inputFile 12
solveDay 13 = Day13.solve $ inputFile 13
solveDay 14 = Day14.solve $ inputFile 14
solveDay 15 = Day15.solve $ inputFile 15
solveDay 16 = Day16.solve $ inputFile 16
solveDay 17 = Day17.solve $ inputFile 17
solveDay 18 = Day18.solve $ inputFile 18
solveDay 19 = Day19.solve $ inputFile 19
solveDay 20 = Day20.solve $ inputFile 20
solveDay 21 = Day21.solve $ inputFile 21
solveDay 22 = Day22.solve $ inputFile 22
solveDay 23 = Day23.solve $ inputFile 23
solveDay 24 = Day24.solve $ inputFile 24
solveDay 25 = Day25.solve $ inputFile 25
solveDay _ = error "Unknown Day!"

displayDay :: Int -> NominalDiffTime -> (String, String) -> String
displayDay n time (x, y) = printf "=== Day %02d ===\nPart 1: %s\nPart 2: %s\nTime: %s\n" n x y (show time)

runDay :: Int -> IO String
runDay n = do
    let fname = inputFile n
    fetchAOCInput fname 2024 n
    start <- getCurrentTime
    result <- solveDay n
    deepseq result $ return () 
    end <- getCurrentTime
    let timeDelta = diffUTCTime end start
    return $ displayDay n timeDelta result

runDays :: [Int] -> IO ()
runDays xs = mapM_ (runDay >=> putStrLn) $ fill xs
  where
    fill [] = [1 .. 25]
    fill xss = xss

main :: IO ()
main = getArgs >>= runDays . map read

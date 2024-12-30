module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import System.FilePath ((</>))

import Day01 (solve)
import Day02 (solve)
import Day03 (solve)
import Day04 (solve)
import Day05 (solve)
import Day06 (solve)
import Day07 (solve)
import Day08 (solve)
import Day09 (solve)
import Day10 (solve)
import Day11 (solve)
import Day12 (solve)
import Day13 (solve)
import Day14 (solve)
import Day15 (solve)
import Day16 (solve)
import Day17 (solve)
import Day18 (solve)
import Day19 (solve)
import Day20 (solve)
import Day21 (solve)
import Day22 (solve)
import Day23 (solve)
import Day24 (solve)
import Day25 (solve)

solveDay :: Int -> FilePath -> IO (String, String)
solveDay 1 = Day01.solve
solveDay 2 = Day02.solve
solveDay 3 = Day03.solve
solveDay 4 = Day04.solve
solveDay 5 = Day05.solve
solveDay 6 = Day06.solve
solveDay 7 = Day07.solve
solveDay 8 = Day08.solve
solveDay 9 = Day09.solve
solveDay 10 = Day10.solve
solveDay 11 = Day11.solve
solveDay 12 = Day12.solve
solveDay 13 = Day13.solve
solveDay 14 = Day14.solve
solveDay 15 = Day15.solve
solveDay 16 = Day16.solve
solveDay 17 = Day17.solve
solveDay 18 = Day18.solve
solveDay 19 = Day19.solve
solveDay 20 = Day20.solve
solveDay 21 = Day21.solve
solveDay 22 = Day22.solve
solveDay 23 = Day23.solve
solveDay 24 = Day24.solve
solveDay 25 = Day25.solve
solveDay _ = error "Unknown Day!"

main :: IO ()
main = defaultMain tests

testedDays :: [Int]
testedDays = [17]

tests :: TestTree
tests = testGroup "All Tests" (testDay <$> testedDays)

testDay :: Int -> TestTree
testDay n = testGroup (printf "day%02d" n) (dayTests n)

dayTests :: Int -> [TestTree]
dayTests 1 = [part1Test 1 "ex1.in" "11", part2Test 1 "ex1.in" "31"]
dayTests 2 = []
dayTests 3 = []
dayTests 4 = []
dayTests 5 = []
dayTests 6 = [part2Test 6 "ex.in" "6"]
dayTests 7 = [part1Test 7 "ex.in" "3749", part2Test 7 "ex.in" "11387"]
dayTests 8 = [part1Test 8 "ex.in" "14", part2Test 7 "ex.in" "11387"]
dayTests 9 = [part1Test 9 "ex.in" "1928", part2Test 9 "ex.in" "2858" ]
dayTests 10 = [part1Test 10 "ex.in" "36", part2Test 10 "ex.in" "81" ]
dayTests 11 = [part1Test 11 "ex.in" "55312"]
dayTests 12 = [part1Test 12 "ex1.in" "140", part1Test 12 "ex2.in" "772", part1Test 12 "ex3.in" "1930", part2Test 12 "ex1.in" "80"]
dayTests 13 = [part1Test 13 "ex.in" "480"]
dayTests 14 = []
dayTests 15 = [part1Test 15 "ex.in" "10092", part2Test 15 "ex.in" "9021"]
dayTests 16 = [part1Test 16 "ex1.in" "7036", part1Test 16 "ex2.in" "11048", part2Test 16 "ex1.in" "45", part2Test 16 "ex2.in" "64"]
dayTests 17 = [part1Test 17 "ex.in" "4,6,3,5,6,3,5,2,1,0", part1Test 17 "ex2.in" "0,1,2", part1Test 17 "ex3.in" "4,2,5,6,7,7,7,7,3,1,0"]
dayTests _ = error "Unknown Day!"

partTest:: ((String, String) -> String) -> Int -> String -> String -> String -> TestTree
partTest f n name path solution = testCase name runTest
    where
        dayString = printf "day%02d" n
        runTest = do

            calcSol <- f <$> solveDay n ("examples" </> dayString </> path)
            calcSol @?= solution

part1Test, part2Test :: Int -> String -> String -> TestTree
part1Test n = partTest fst n "part1" 
part2Test n = partTest snd n "part2"


{-# LANGUAGE OverloadedStrings #-}
module Day07 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text

data Equation = Equation {lhs::Int, rhs::[Int]} deriving (Eq, Show)
type Input = [Equation]
type Op = Int -> Int -> Int

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseLine <$> T.lines txt
    return (show $ part1 input, show $ part2 input)

parseLine txt = Equation (readT num) (nums)
    where [num, rest] = T.splitOn ": " txt
          nums = reverse $ readT <$> T.splitOn " " rest

isSolvable :: [Op] -> Equation -> Bool
isSolvable ops (Equation lhs rhs) = lhs `elem` (allNums (reverse rhs))
    where allNums [x] = [x]
          allNums (x:xs) = filter (<= lhs) $ ($ x) <$> ops <*> allNums xs

isSolvableFast :: Equation -> Bool
isSolvableFast (Equation _ []) = False
isSolvableFast (Equation lhs [x]) = lhs == x
isSolvableFast (Equation lhs (x:xs)) = isSolvableFast (Equation (lhs-x) xs) || (m == 0 && isSolvableFast (Equation d xs))
    where (d, m) = lhs `divMod` x

runOps :: [Op] -> [Equation] -> Int
runOps ops = sum . map lhs . filter isSolvableFast

(><) :: Op
x >< y = read $ show x <> show y

part1, part2 :: Input -> Int
part1 = runOps [(*), (+)]
part2 = const 0

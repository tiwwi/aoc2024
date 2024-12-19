{-# LANGUAGE OverloadedStrings #-}
module Day07 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text

data Equation = Equation {lhs::Int, rhs::[Int]} deriving (Eq, Show)
type Input = [Equation]

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseLine <$> T.lines txt
    return (show $ part1 input, show $ part2 input)

parseLine :: T.Text -> Equation
parseLine txt = Equation (readT num) (nums)
    where [num, rest] = T.splitOn ": " txt
          nums = reverse $ readT <$> T.splitOn " " rest


solvable, solvable2 :: Equation -> Bool
solvable (Equation lhs [x]) = lhs == x
solvable (Equation lhs (x:xs)) = solvable (Equation (lhs-x) xs) || (m == 0 && solvable (Equation d xs))
    where (d, m) = lhs `divMod` x

solvable2 (Equation lhs [x]) = lhs == x
solvable2 (Equation lhs (x:xs)) = (solvable2 (Equation (lhs-x) xs)) || ((m == 0 && solvable2 (Equation d xs))) || (or $ solvable2 <$> (Equation <$> intStripSuffix x lhs <*> pure xs))
    where (d, m) = lhs `divMod` x
          

intStripSuffix :: Int -> Int -> Maybe Int
intStripSuffix 0 y = Just y
intStripSuffix x y 
     | x == 0 = Just y
     | lastx == lasty = intStripSuffix remx remy
     | otherwise = Nothing
  where (remx, lastx) = x `divMod` 10
        (remy, lasty) = y `divMod` 10

part1, part2 :: Input -> Int
part1 = sum . map lhs . filter solvable
part2 = sum . map lhs . filter solvable2


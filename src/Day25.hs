{-# LANGUAGE OverloadedStrings #-}
module Day25 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Matrix
import Data.List
import Data.Bifunctor

type Schematic = UArray Pos Bool

type SmallSchema = [Int]

newtype Key = Key SmallSchema
newtype Lock = Lock SmallSchema

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = map parseInput (T.splitOn "\n\n" txt)
    return (show $ part1 input, part2)

parseInput :: T.Text -> Schematic
parseInput =  boolify . (readAOCMatrix :: String -> UArray Pos Char) . T.unpack 
    where boolify arr = listArray (bounds arr) $ (=='#') <$> elems arr

isLock :: Schematic -> Bool
isLock arr = arr ! (V2 1 1)

shrink :: (Bool -> Bool) -> Schematic -> SmallSchema
shrink p lock = [length $ takeWhile p (column c') | c' <- [1..c]]
    where (V2 _ _, V2 r c) = bounds lock
          column c' = [lock ! V2 r' c' | r' <- [1..r]]

shrinkLock :: Schematic -> Lock
shrinkLock = Lock . shrink id
shrinkKey :: Schematic -> Key
shrinkKey = Key . shrink not

isMatch :: Lock -> Key -> Bool
isMatch (Lock ls) (Key ks) = and $ zipWith (<=) ls ks

part1 :: [Schematic] -> Int
part1 arrs = sum $ fromEnum <$> liftA2 isMatch locks keys
    where (locks, keys) = bimap (map shrinkLock) (map shrinkKey) $ partition isLock arrs
part2 :: String
part2 = "Frohe Weihnachten!"

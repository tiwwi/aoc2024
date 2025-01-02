module Day22 (solve) where

import Data.Bits
import qualified Data.Map.Strict as M

type Input = [Int]
type Sequence = (Int, Int, Int, Int)


solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- readFile fname
    let input = parseInput txt
    return (show $ part1 input, show $ part2 input)

parseInput :: String -> [Int]
parseInput = map read . lines

nextNumber :: Int -> Int
nextNumber n = n'''
    where n' = prune $ mix (64*n) n
          n'' = prune $ mix (n' `div` 32) n'
          n''' = prune $ mix (n''*2048) n''

mix :: Int -> Int -> Int
mix a b = a `xor` b

prune :: Int -> Int
prune a = a `mod` 16777216

part1 :: Input -> Int
part1 = sum . map twot
    where twot = (!! 2000) . iterate nextNumber
part2 :: Input -> Int
part2 input = maximum $ M.unionsWith (+) $ go1 <$> input
    where go1 n = M.fromList $ reverse $ seqsWithPrice n

sequences :: [d] -> [(d, d, d, d)]
sequences (a:rest@(b:(c:(d:xs)))) = (a,b,c,d):sequences rest
sequences _ = []

seqsWithPrice :: Int -> [(Sequence, Int)]
seqsWithPrice n = zip (sequences subs) (drop 4 prices)
    where numbers = take 2001 . iterate nextNumber $ n
          prices = (`mod` 10) <$> numbers
          subs = zipWith (-) (tail prices) prices
          

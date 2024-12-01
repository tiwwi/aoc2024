module Helpers (
    readT,
    readMaybeT,
    decimalToInt,
    quickParseT,
    count,
    takeUntil,
    pairs,
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Text.Read (readMaybe)
import Data.List (tails)

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

decimalToInt :: [Int] -> Int
decimalToInt = foldl go 0
    where go acc x = 10*acc + x

quickParseT :: T.Parser a -> T.Text -> a
quickParseT p txt = case T.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
    | f x = [x]
    | otherwise = x:takeUntil f xs

pairs :: [a] -> [(a, a)]
pairs xs = [ (x,y) | (x:ys) <- tails xs, y <- ys ]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

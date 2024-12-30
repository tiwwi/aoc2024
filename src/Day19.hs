{-# LANGUAGE OverloadedStrings #-}

module Day19 (solve) where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Vector qualified as V
import Helpers.ByteString
import Helpers.List (count)
import Data.Set qualified as S

type Towel = B.ByteString
type Design = B.ByteString

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- B.readFile fname
  let (_towels, designs) = quickParseB inputP txt
      maxTowel = maximum $ B.length <$> _towels
      towels = S.fromList _towels
      nums = findNum maxTowel towels <$> designs
  return (show $ count (/= 0) nums, show $ sum nums)

inputP :: A.Parser ([Towel], [Design])
inputP =
  (,)
    <$> (towelP `A.sepBy1` A.string ", ")
    <* A.skipSpace
    <*> (A.sepBy1 designP A.skipSpace)

towelP :: A.Parser Towel
towelP = A.takeWhile1 isAlpha

designP :: A.Parser Design
designP = towelP

findNum :: Int -> S.Set Towel -> Design -> Int
findNum maxTowel towels design = dpTable V.! B.length design
  where
    dpTable = V.generate (B.length design + 1) indexF
    tailsV = V.fromList $ reverse $ B.tails design
    indexF 0 = 1
    indexF i =
      let subDesign = tailsV V.! i
          options = sum $ [dpTable V.! (i - cutLength) | (cutLength, prefix) <- zip [1..maxTowel] (tail $ B.inits subDesign), S.member prefix towels]
       in options

{-# LANGUAGE OverloadedStrings #-}

module Day19 (solve) where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Vector qualified as V
import Helpers.ByteString
import Helpers.List (count)

type Towel = B.ByteString
type Design = B.ByteString

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- B.readFile fname
  let (towels, designs) = quickParseB inputP txt
      nums = findNum towels <$> designs
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

findNum :: [Towel] -> Design -> Int
findNum towels design = dpTable V.! B.length design
  where
    dpTable = V.generate (B.length design + 1) indexF
    tailsV = V.fromList $ reverse $ B.tails design
    indexF 0 = 1
    indexF i =
      let subDesign = tailsV V.! i
          options = sum $ [dpTable V.! (i - B.length towel) | towel <- towels, B.isPrefixOf towel subDesign]
       in options

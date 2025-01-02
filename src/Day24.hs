{-# LANGUAGE OverloadedStrings #-}
module Day24 (solve) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A
import Helpers.ByteString
import Control.Applicative
import Data.Bits
import qualified Data.Map as M
import Text.Printf

type Id = B.ByteString
data Op = AND | OR | XOR deriving (Eq, Show, Ord)
data Value = Fixed Bool | Calc Op Id Id deriving (Eq, Show, Ord)
data Wire = Wire {name::B.ByteString, value::Value} deriving (Show, Eq, Ord)

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <-  B.readFile fname
    let (set, var) = parseInput txt
        results = evaluateWires (set <> var)
        part1r = collect 'z' results
    return (show $ part1r, show $ part2 set var)

parseInput = quickParseB inputP

inputP :: A.Parser ([Wire], [Wire])
inputP = (,) <$> A.sepBy1 constWireP  A.endOfLine <*> (A.skipSpace *> A.sepBy1 calcWireP A.endOfLine)

constWireP :: A.Parser Wire
constWireP = Wire <$> idP <*> (A.string ": " *> (Fixed . (==1) <$> A.decimal))

calcWireP :: A.Parser Wire
calcWireP = do
    id1 <- idP
    A.skipSpace
    op <- opP
    A.skipSpace
    id2 <- idP
    _ <- A.string " -> "
    wire <- idP
    pure $ Wire wire $ Calc op id1 id2

idP :: A.Parser Id
idP = A.take 3

opP :: A.Parser Op
opP = (OR <$ A.string "OR") <|> (AND <$ A.string "AND") <|> (XOR <$ A.string "XOR")

runOp :: Op -> Bool -> Bool -> Bool
runOp AND = (&&)
runOp OR = (||)
runOp XOR = xor

evaluateWires :: [Wire] -> M.Map Id Bool
evaluateWires wires = results
    where results = M.fromList $ map evaluateWire wires
          evaluateWire (Wire id (Fixed b)) = (id, b)
          evaluateWire (Wire id (Calc op i1 i2)) = (id, runOp op (results M.! i1) (results M.! i2))

bitsToInt :: [Bool] -> Int
bitsToInt = sum . zipWith (*) (map bit [0..]) . map fromEnum


collect c wireMap = bitsToInt [b | (x,b) <- M.assocs wireMap, B.head x == c]
-- Look at this and then do it by hand :)
part2 :: [Wire] -> [Wire] -> [(Int, Int, Int, Int, Int)]
part2 set var = [(i, x, y, correct, wrong) | i <- [0..size-1], x <- [0, bit i], y <- [0, bit i], let correct = x+y, let wrong = x `calc` y, correct /= wrong]
    where size = (length $ set) `div` 2
          calc x y = collect 'z' $ evaluateWires (toWire 'x' x <> toWire 'y' y <> var)
          toWire c n = wireBit c n <$> [0..size-1]
          wireBit c n i = Wire (B.pack $ c:printf "%02d" i) (Fixed $ testBit n i)

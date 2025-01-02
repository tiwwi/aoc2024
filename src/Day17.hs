{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Day17 (solve) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.Attoparsec.Text qualified as A
import Data.Bits
import Data.Char (isDigit)
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed qualified as V
import Helpers.Text

data Memory = Memory {_a :: Int, _b :: Int, _c :: Int} deriving (Show)

makeLenses ''Memory

data Computer = Computer {_mem :: Memory, _pointer :: Int}

makeLenses ''Computer

type CompCalc = RWS Program (Endo [Int]) Computer

type Program = V.Vector Int

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- T.readFile fname
  let (mem, program) = parseInput txt
  return (part1 mem program, show $ part2 mem program)

parseInput = quickParseT inputP

inputP :: A.Parser (Memory, V.Vector Int)
inputP = (,) <$> memP <*> (A.skipWhile (not . isDigit) *> (V.fromList <$> (A.decimal `A.sepBy1` A.char ',')))

memP :: A.Parser Memory
memP = Memory <$> (next A.decimal) <*> (next A.decimal) <*> (next A.decimal)

next :: A.Parser a -> A.Parser a
next p = p <|> (A.anyChar *> next p)

getR :: Lens' Memory Int -> CompCalc Int
getR r = gets $ (^. mem . r)

setR :: Lens' Memory Int -> Int -> CompCalc ()
setR r v = modify $ (mem . r) .~ v

modifyR :: Lens' Memory Int -> (Int -> Int) -> CompCalc ()
modifyR r f = modify $ (mem . r) %~ f

combo :: Int -> CompCalc Int
combo 4 = getR a
combo 5 = getR b
combo 6 = getR c
combo 7 = error "Invalid Combo Operand!"
combo n = pure n

literal :: Int -> CompCalc Int
literal = pure
output :: Int -> CompCalc ()
output n = tell (Endo ([n]++))

advancePtr :: CompCalc ()
advancePtr = modify $ pointer %~ (+ 2)

rDiv :: Lens' Memory Int -> Int -> CompCalc ()
rDiv r opr = do
    aVal <- getR a
    val <- combo opr
    setR r $ shift aVal (-val)
    advancePtr

runOP :: Int -> Int -> CompCalc ()
runOP 0 opr = rDiv a opr
runOP 1 opr = literal opr >>= (\val -> modifyR b (xor val)) >> advancePtr
runOP 2 opr = combo opr >>= (\val -> setR b $ val `mod` 8) >> advancePtr
runOP 3 opr = do aVal <- getR a; if aVal /= 0 then literal opr >>= (\val -> modify $ pointer .~ val) else advancePtr
runOP 4 _opr = liftA2 xor (getR b) (getR c) >>= setR b >> advancePtr
runOP 5 opr = combo opr >>= (output . (`mod` 8)) >> advancePtr
runOP 6 opr = rDiv b opr
runOP 7 opr = rDiv c opr
runOP _ _opr = error "invalid opcode"

runCalc :: CompCalc ()
runCalc = do
    ptr <- gets $ (^. pointer)
    programSize <- asks V.length
    when (ptr+1 < programSize) $ do
        instr <- asks (V.! ptr)
        opr <- asks (V.! (ptr + 1))
        runOP instr opr
        runCalc

getSolutions :: Memory -> Program -> [Int]  -> [Int]
getSolutions _mem _program [] = [0]
getSolutions mem program (now:rest) = options
    where options = filter (\num -> (head $ runProgram (mem {_a = num}) program) == now) [shift k 3 + num | k <- known, num <- [0..7]] 
          known = getSolutions mem program rest

runProgram :: Memory -> Program -> [Int]
runProgram mem program = appEndo result $ []
    where (_, _, result) =runRWS runCalc program (Computer mem 0)

part1 :: Memory -> Program -> String
part1 mem program = T.unpack $ (T.intercalate ",") $ map (T.pack . show) output
    where output = runProgram mem program
part2 :: Memory -> Program -> Int
part2 mem program = head $ getSolutions mem program prog
    where prog = V.toList program

{-# LANGUAGE OverloadedStrings #-}

module Day03 (solve) where

import Control.Applicative
import Data.Attoparsec.Text qualified as A
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text (quickParseT)

type Input = [Op]

data Op = Mul Int Int | Do | Dont deriving (Eq, Show)

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- T.readFile fname
  let input = parseInput txt
  return (show $ part1 input, show $ part2 input)

parseInput :: T.Text -> [Op]
parseInput = quickParseT $ A.many1 (next opParser)

opParser :: A.Parser Op
opParser = mulParser <|> (Do <$ A.string "do()") <|> (Dont <$ A.string "don't()")
    where mulParser = Mul
            <$> (A.string "mul" *> A.char '(' *> A.decimal <* A.char ',')
            <*> (A.decimal <* A.char ')')

next :: A.Parser a -> A.Parser a
next p = p <|> (A.anyChar *> next p)

eval :: Op -> Int
eval (Mul a b) = a * b
eval Do = 0
eval Dont = 0

part1 :: [Op] -> Int
part1 = sum . map eval

part2 :: Input -> Int
part2 = snd . foldl go (True, 0)
  where
    go (True, s) (Mul a b) = (True, s + (a * b))
    go (False, s) (Mul _ _) = (False, s)
    go (_, s) op = (op == Do, s)

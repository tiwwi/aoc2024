{-# LANGUAGE OverloadedStrings #-}
module Day13 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Attoparsec.Text qualified as A
import Linear
import Helpers.Text (quickParseT)
import Data.Ratio
import Control.Monad (guard)
import Data.Maybe (mapMaybe)

data Claw = Claw {a:: V2 Int, b :: V2 Int, prize :: V2 Int} deriving(Eq, Show)

type Input = [Claw]

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseInput txt
    return (show $ part1 input, show $ part2 input)

parseInput :: T.Text -> [Claw]
parseInput = quickParseT (clawP `A.sepBy1` A.endOfLine)
clawP :: A.Parser Claw
clawP = Claw <$>
    (V2 <$> (A.string "Button A: X+" *> A.decimal) <*> (A.string ", Y+" *> A.decimal) <* A.endOfLine) <*>
    (V2 <$> (A.string "Button B: X+" *> A.decimal) <*> (A.string ", Y+" *> A.decimal) <* A.endOfLine) <*>
    (V2 <$> (A.string "Prize: X=" *> A.decimal) <*> (A.string ", Y=" *> A.decimal) <* A.endOfLine)

isInteger :: Ratio Int -> Bool
isInteger = (==1) . denominator

buttonMatrix :: Claw -> V2 (V2 (Ratio Int))
buttonMatrix (Claw (V2 ax ay) (V2 bx by) _) = V2 (fromIntegral <$> V2 ax bx) (fromIntegral <$> V2 ay by)

solveClaw :: Claw -> Maybe (V2 Int)
solveClaw c@(Claw _ _ t) = (guard $ all (\x -> isInteger x && x >= 0) solution) >> (return $ fmap numerator solution)
    where solution = inv22 (buttonMatrix c) !* (fromIntegral <$> t)

findPresses :: Input -> Int
findPresses = sum . map (\(V2 x y) -> 3*x + 1*y) . mapMaybe solveClaw


part1 :: Input -> Int
part1 = findPresses
    where 
part2 :: Input -> Int
part2 = findPresses . map (\c -> c {prize=prize c + V2 m m})
    where m = 10000000000000

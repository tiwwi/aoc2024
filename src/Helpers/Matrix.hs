module Helpers.Matrix
  ( AOCMatrix,
    readAOCMatrix,
    showMatrix,
    Pos,
    Dir,
    up,
    fromArrow,
    right,
    down,
    left,
    rotateRight,
    rotateLeft,
    cardinalNbs,
    toVec,
    cardinals,
    module Linear.V2,
    module Linear.Vector,
    module Data.Array.IArray,
    module Data.Array.Unboxed,
  )
where

import Data.Array.IArray
import Data.Array.Unboxed
import Linear.V2
import Linear.Vector
import Data.Maybe (mapMaybe)

data Direction = U | R | D | L deriving (Show, Eq, Ord, Enum)

type Pos = V2 Int
type Dir = V2 Int

type AOCMatrix = UArray Pos Char

readAOCMatrix :: IArray a Char => String -> a Pos Char
readAOCMatrix txt = listArray (lo, hi) $ concat lns
  where
    nCols = length $ head lns
    nRows = length lns
    lns = lines txt
    lo = V2 1 1
    hi = V2 nRows nCols

up, right, down, left :: V2 Int
up = V2 (-1) 0
right = V2 0 1
down = V2 1 0
left = V2 0 (-1)

cardinals = [up, right, down, left] :: [Dir]

fromArrow :: Char -> Maybe Dir
fromArrow '^' = Just up
fromArrow '>' = Just right
fromArrow 'v' = Just down
fromArrow '<' = Just left
fromArrow _ = Nothing


toVec :: Direction -> Dir
toVec U = V2 (-1) 0
toVec R = V2 0 1
toVec D = V2 1 0
toVec L = V2 0 (-1)

rotateRight, rotateLeft :: V2 Int -> V2 Int
rotateRight (V2 a b) = V2 b (-a)
rotateLeft (V2 a b) = V2 (-b) a

cardinalNbs :: IArray a e => a Pos e -> Pos -> [(Pos, e)]
cardinalNbs arr pos = mapMaybe (\p -> (p,) <$> (arr !? p)) $ (pos +) <$> cardinals

paddedWith :: (IArray a e) => a Pos e -> e -> a Pos e
paddedWith arr pad = array (lo', hi') (zip square (repeat pad) ++ assocs arr)
    where (lo@(V2 lx ly), hi@(V2 hx hy)) = bounds arr
          lo' = subtract 1 <$> lo
          hi' = (+1) <$> hi
          square = [V2 (hx+1) y | ]++ [V2 x (hy + 1) | x <- [lx..hx+1]]++ [V2 x (ly - 1) | x <- [lx..hx+1]]++ [V2 (lx-1) y | y <- [ly-1 .. ly+1]]

showMatrix :: (Show e, IArray a e) => a Pos e -> String
showMatrix arr = unlines $ [row i | i <- [lx..hx]]
    where row i = concatMap show [arr ! V2 i j | j <- [ly..hy]]
          (V2 lx ly, V2 hx hy) = bounds arr

{-# LANGUAGE OverloadedStrings #-}
module Day15 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Matrix
import Data.Maybe

type Input = [T.Text]
type Warehouse = Array Pos Field
data Field = Wall | Robot | Box | Empty deriving(Eq)

instance Show Field where
    show Wall = "#"
    show Robot = "@"
    show Box = "O"
    show Empty = "."

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let (ware, moves) = parseInput txt
    return (show $ part1 ware moves, show $ part2 undefined)

charToField :: Char -> Field
charToField '#' = Wall
charToField '@' = Robot
charToField 'O' = Box
charToField '.' = Empty
charToField _ = error "no"

parseInput :: T.Text -> (Warehouse, [Dir])
parseInput txt = (ware, moves)
    where (mat, rest) =  (T.breakOn "\n\n" txt)
          moves = mapMaybe fromArrow $ T.unpack $ T.strip rest
          ware = fmap charToField $ readAOCMatrix $ T.unpack $ T.strip mat

padded :: Warehouse -> Warehouse
padded ware = array (V2 (lx -1) (ly -1), V2 (hx + 1) (hy + 1))
    where (V2 lx ly, V2 hx hy) = bounds ware

part1 :: Warehouse -> [Dir] -> String
part1 ware moves = "undefined"
part2 :: Input -> String
part2 = const "unfinished"

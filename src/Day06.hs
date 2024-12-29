module Day06 (solve) where

import qualified Data.Set as S
import Helpers.Matrix
import Helpers.List
import Data.Tuple

type Board = UArray Pos Field

data Guard = Guard {pos::Pos, dir::Dir} deriving (Show, Eq, Ord)
type Field = Bool
clear, wall :: Bool
clear = False
wall = True

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- readFile fname
    let (guardo, board) = parseInput txt
    return (show $ part1 board guardo, show $ part2 board guardo)

parseInput :: String -> (Guard, Board)
parseInput str = (Guard startPos up, listArray (bounds arr) $ charToField <$> elems arr)
    where arr = readAOCMatrix str :: UArray Pos Char
          Just startPos = lookup '^' $ swap <$> assocs arr
          charToField '#' = wall
          charToField _ = clear

step :: Board -> Guard -> Maybe Guard
step board (Guard pos dir) = next >>= nextPos
  where tryNext = pos + dir
        next = board !? tryNext
        nextPos False = pure $ Guard tryNext dir
        nextPos True = step board (Guard pos (rotateRight dir))

walkPath :: Board -> Guard -> [Guard]
walkPath board = takeWhileJust . iterate (>>= (step board)) . pure

loops :: Board -> Guard -> Bool
loops board guardo = go S.empty $ walkPath board guardo
  where go _ [] = False
        go set (x:xs) = S.member x set || go (S.insert x set) xs

part1, part2 :: Board -> Guard -> Int
part1 board = countUnique . map pos . walkPath board
part2 board guardo = count (\pos -> loops (board // [(pos, wall)]) guardo) $ toTry
    where startPos = pos guardo
          toTry = dedup [ pos | Guard pos _ <- walkPath board guardo, pos /= startPos]

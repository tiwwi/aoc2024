{-# LANGUAGE OverloadedStrings #-}
module Day15 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text
import Helpers.Matrix
import Data.Maybe
import Control.Monad.ST
import Data.Array.ST
import Control.Monad
import Data.STRef.Strict
import Data.Tuple
import Data.List

type Warehouse = Array Pos Field
data Field = Wall | Robot | SBox | LBox | RBox | Empty deriving(Eq)

instance Show Field where
    show Wall = "#"
    show Robot = "@"
    show SBox = "O"
    show LBox = "["
    show RBox = "]"
    show Empty = "."

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let (ware, ware2, moves) = parseInput txt
    return (show $ part ware moves, show $ part ware2 moves)

charToField :: Char -> Field
charToField '#' = Wall
charToField '@' = Robot
charToField 'O' = SBox
charToField '[' = LBox
charToField ']' = RBox
charToField '.' = Empty
charToField _ = error "no"

parseInput :: T.Text -> (Warehouse, Warehouse, [Dir])
parseInput txt = (ware, ware', moves)
    where (mat, rest) =  (T.breakOn "\n\n" txt)
          moves = mapMaybe fromArrow $ T.unpack $ T.strip rest
          ware = fmap charToField $ readAOCMatrix $ T.unpack $ T.strip mat
          ware' = fmap charToField $ readAOCMatrix $ T.unpack $ replaceAll replacements $ T.strip mat
          replacements = [("#","##"),("O","[]"),("@","@."),(".","..")]

getObjCoords :: Pos -> Field -> [Pos]
getObjCoords pos LBox = [pos, pos + right]
getObjCoords pos RBox = [left + pos, pos]
getObjCoords pos _ = [pos]

(|*|) :: V2 Int -> V2 Int -> Int
(|*|) a b = sum $ a*b

canMoveObj :: STArray s Pos Field -> Dir -> Pos -> ST s Bool
canMoveObj ware dir pos = do
    curObj <- readArray ware pos
    case curObj of
        Empty -> pure True
        Wall  -> pure False
        _     -> and <$> mapM (canMoveObj ware dir) (filter (/= pos) $ (dir+) <$> getObjCoords pos curObj)

forceMoveObj :: STArray s Pos Field -> STRef s Pos -> Dir -> Pos -> ST s ()
forceMoveObj ware robopos dir pos = do
    curObj <- readArray ware pos
    when (curObj /= Empty && curObj /= Wall) $ do
        let coords = getObjCoords pos curObj
            coordsByDir = reverse $ sortOn (|*| dir) coords
        mapM_ (forceMoveObj ware robopos dir) (filter (`notElem` coords) $ map (+dir) coords)
        forM_ coordsByDir $ \pos' -> do
            curObj' <- readArray ware pos'
            writeArray ware (pos' + dir) curObj'
            writeArray ware pos' Empty
    when (curObj == Robot) $ writeSTRef robopos (pos + dir)


runInstructions :: Warehouse -> [Dir] -> Warehouse
runInstructions ware moves = runST $ do
    let Just initialRobo = lookup Robot $ map swap $ assocs ware
    roboPos <- newSTRef initialRobo
    wares <- thaw ware
    forM_ moves $ \dir -> do
        curRobo <- readSTRef roboPos
        canMove <- canMoveObj wares dir curRobo
        when canMove $ forceMoveObj wares roboPos dir curRobo
    freeze wares

part :: Warehouse -> [Dir] -> Int
part ware moves = sum $ [100*(x-1)+(y-1) | (V2 x y, b) <- assocs afterMoves, (b `elem` [LBox, SBox])]
    where afterMoves = runInstructions ware moves

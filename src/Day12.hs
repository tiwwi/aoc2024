module Day12 (solve) where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.IntMap qualified as M
import Data.STRef.Strict
import Helpers.List (count)
import Helpers.Matrix
import Linear.V3
import Data.Maybe (fromMaybe)

type Board = UArray Pos Char

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- readFile fname
  let input = readAOCMatrix $ txt :: Board
      (part1, part2) = parts input
  return (show $ part1, show $ part2)

findAreas :: Board -> UArray Pos Int
findAreas board = runST $ do
  groups <- newArray (bounds board) (-1) :: ST s (STUArray s Pos Int)
  groupCount <- newSTRef 0
  mapM_ (markNewGroup groups groupCount) (indices board)
  freeze groups
  where
    markNewGroup groups groupCount pos = do
      pgrp <- readArray groups pos
      when (pgrp == (-1)) $ do
        curGroup <- readSTRef groupCount
        let curSymbol = board ! pos
        markGroup groups curGroup curSymbol pos
        modifySTRef groupCount (+ 1)

    markGroup groups curGroup curSymbol pos = do
      pgrp <- readArray groups (pos)
      when (pgrp == (-1)) $ do
        writeArray groups pos curGroup
        let inGroupNbs = [p | (p, v) <- cardinalNbs board pos, v == curSymbol]
        mapM_ (markGroup groups curGroup curSymbol) inGroupNbs

analyzeAreas :: UArray Pos Int -> M.IntMap (V3 Int)
analyzeAreas groups = M.fromListWith (+) $ [(g, V3 1 (fenceAt p) (cornerAt p)) | (p, g) <- assocs groups]
  where
    fenceAt pos = let cpos = groups ! pos in 4 - (length $ [() | (_, v) <- cardinalNbs groups pos, v == cpos])
    cornerAt pos = count isCorner diags
      where
        cpos = groups ! pos
        diags = [V2 (-1) 1, V2 (-1) (-1), V2 1 (-1), V2 1 1]
        isCorner diag = let one = getSafe $ groups !? (pos + diag * down); two = getSafe $ groups !? (pos + diag * right); both = getSafe $ groups !? (pos + diag) in (one == cpos && two == cpos && both /= cpos) || (one /= cpos && two /= cpos)
        getSafe = fromMaybe (-1)

-- A comment
parts :: Board -> (Int, Int)
parts board = (sum $ (\(V3 area perim _) -> area*perim) <$> areas,
               sum $ (\(V3 area _ corners) -> area * corners) <$> areas)
    where areas = analyzeAreas $ findAreas board

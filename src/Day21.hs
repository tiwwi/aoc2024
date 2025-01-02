module Day21 (solve) where

import Helpers.Matrix hiding (ey, ex)
import Data.Maybe
import qualified Data.Map as M
import Data.Char (isDigit)

type Button = Char
type Pad = Array Pos (Maybe Button)

toButton :: Char -> Maybe Char
toButton '.' = Nothing
toButton x = Just x

numpadStr, ctrlpadStr, myButtons :: String
numpadStr = "789\n456\n123\n.0A"
ctrlpadStr = ".^A\n<v>"
myButtons  = "A^v<>"

numPad, ctrlPad :: Array Pos (Maybe Button)
numPad = toButton <$> readAOCMatrix numpadStr
ctrlPad = toButton <$> readAOCMatrix ctrlpadStr

solve :: FilePath -> IO (String,  String)
solve fname = do
    txt <- readFile fname
    let input = lines txt
    return (show $ botsWith 2 input, show $ botsWith 25 input)

paths :: Pad -> Pos -> Pos -> [[Button]]
paths pad s@(V2 sx sy) e@(V2 ex ey) 
    | s == e = [['A']]
    | otherwise = xpaths ++ ypaths
    where dirx = signum $ ex - sx
          diry = signum $ ey - sy
          xButton 1 = 'v'
          xButton (-1) = '^'
          xButton _ = error "shut up hls"
          yButton 1 = '>'
          yButton (-1) = '<'
          yButton _ = error "shut up hls"
          nextX = V2 (sx+dirx) sy
          nextY = V2 sx (sy+diry)
          xpaths = if dirx /= 0 && isJust (pad ! nextX) then (xButton dirx:) <$> paths pad nextX e else []
          ypaths = if diry /= 0 && isJust (pad ! nextY) then (yButton diry:) <$> paths pad nextY e else []



costMaps :: [Pad] -> M.Map (Button, Button) Int
costMaps [] = M.fromList [((b1, b2), 1) | b1 <- myButtons, b2 <- myButtons]
costMaps (a:as) = updated
    where buttons = [(p, b') | (p, b) <- assocs a, Just b' <- pure b]
          mp = costMaps as
          updated = M.fromList [((b1, b2), minCost p1 p2) | (p1, b1) <- buttons, (p2, b2) <- buttons]
          minCost b1 b2 = minimum $ pathCost <$> paths a b1 b2
          pathCost = pathCost' . ('A':)
          pathCost' [_] = 0
          pathCost' (x:y:xs) = (mp M.! (x, y)) + pathCost' (y:xs)

botsWith :: Int -> [[Button]] -> Int
botsWith n = sum . map complexity
    where complexity = (*) <$> (read . takeWhile isDigit) <*> getLens
          getLens xs = sum $ ((partMap M.!)) <$> (pairs $ 'A':xs)
            where pairs xs' = zip xs' $ tail xs'
          partMap = costMaps $ numPad:(replicate n ctrlPad)

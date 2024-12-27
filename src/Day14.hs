{-# LANGUAGE OverloadedStrings #-}
module Day14 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Attoparsec.Text qualified as A
import Helpers.Text
import Helpers.Matrix
import Helpers.List (counter)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (forM_)

type Input = [Robot]
data Robot = Robot Pos Dir

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = parseInput txt
    --part2 input
    return (show $ part1 input, "Look at aocDay14.txt -- (It's frame 6620) :)")

parseInput :: T.Text -> Input
parseInput = quickParseT (botP `A.sepBy1` A.endOfLine)

botP :: A.Parser Robot
botP = Robot 
    <$> (V2 <$> (A.string "p=" *> A.signed A.decimal) <*> (A.string "," *> A.signed A.decimal)) <* A.space
    <*> (V2 <$> (A.string "v=" *> A.signed A.decimal) <*> (A.string "," *> A.signed A.decimal))

botMove :: V2 Int -> Int -> Robot -> Robot
botMove bds n (Robot pos dir) = Robot (clamp bds $ pos + n *^ dir) dir
    where clamp (V2 xbd ybd) (V2 a b) = V2 (a `mod` xbd) (b `mod` ybd)

quadrants :: (V2 Int) -> Robot -> V2 Int
quadrants bds (Robot pos _) = signum <$> pos - mids
    where mids = (`div` 2) <$> bds

drawBots :: V2 Int -> [Robot] -> T.Text
drawBots bds@(V2 x y) bots = T.unlines $ [row j | j <- [0..y-1]]
    where botArray = listArray arrayBds ('.' <$ range arrayBds) // [(pos, '#') | Robot pos _ <- bots]:: UArray Pos Char
          arrayBds = (V2 0 0, bds - V2 1 1)
          row j = T.pack $ [botArray ! V2 i j | i <- [0..x-1]]

aocBound = V2 101 103

part1 :: Input -> Int
part1 = product . (flip M.restrictKeys keysSet) . counter . map (quadrants bounds . botMove bounds 100)
    where bounds = V2 101 103
          keysSet = S.fromList [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) (-1)] 

part2 :: Input -> IO ()
part2 bots = do
    let nFrame = 10000
        botsEach = zip [0..] $ take nFrame $ map (drawBots aocBound) $ iterate (map $ botMove aocBound 1) bots
    mapM_ (uncurry drawFrame) botsEach

drawFrame :: Int -> T.Text -> IO ()
drawFrame n t = do
    T.appendFile "aocDay14.txt" ("FRAME: " <> (T.pack $ show n))
    T.appendFile "aocDay14.txt" "\n"
    T.appendFile "aocDay14.txt" t
    T.appendFile "aocDay14.txt" "\n\n\n"

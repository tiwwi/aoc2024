module Day11 (solve) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Helpers.Text(readT)
import Data.IntMap qualified as M

type Stone = Int
type Input = [Stone]

type Stones = M.IntMap Int

solve :: FilePath -> IO (String, String)
solve fname = do
    txt <- T.readFile fname
    let input = readT <$> (T.words . T.strip $ txt)
        (part1, part2) = parts input
    return (show $ part1, show $ part2)

-- Is this faster than using div? I dunno
nDigits :: Int -> Int
nDigits = findTen' 10 1
    where findTen' ten k n
                | ten > n = k
                | otherwise = findTen' (ten*10) (k+1) n

blink :: Stones -> Stones
blink stones = M.fromListWith (+) (M.toList stones >>= (\(stone, n) -> (,n) <$> blinkOne stone)) 

blinkOne :: Stone -> [Stone]
blinkOne stone 
    | stone == 0 = [1]
    | even $ digits = splitStone
    | otherwise = [stone*2024]
  where digits = nDigits stone
        splitStone = listify $ stone `divMod` (10 ^ (digits `div` 2))
        listify (x,y) = [x,y]

parts :: Input -> (Int, Int)
parts inp = (countStones part1, countStones part2)
    where countStones = sum . M.elems 
          blinkSeq = iterate blink . M.fromList . flip zip (repeat 1) $ inp
          part1 = blinkSeq !! 25
          part2 = blinkSeq !! 75


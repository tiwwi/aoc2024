module Day09 (solve) where

import Data.Char (digitToInt)
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Maybe (fromMaybe)

data Block = Block {size :: Int, content :: Content} deriving (Eq)
data Content = Null | Full Int deriving (Eq)
type Storage = Seq.Seq Block

instance Show Block where
    show (Block s content) = (concat $ replicate s $ show content) <> "|"

instance Show Content where
    show Null = "."
    show (Full nr) = show nr

solve :: FilePath -> IO (String, String)
solve fname = do
  txt <- T.readFile fname
  let input = blockify $ map digitToInt $ T.unpack $ T.strip txt
  return (show $ part1 input, show $ part2 input)

blockify :: [Int] -> Storage
blockify = go . zip [0 ..]
  where
    go [] = Seq.Empty
    go ((nr, s) : xs) =
      let (d, m) = nr `divMod` 2
       in Block s (if m == 0 then Full d else Null) :<| go xs

isEmpty :: Block -> Bool
isEmpty (Block _ Null) = True
isEmpty _ = False

compress :: Storage -> Storage
compress Seq.Empty = Seq.Empty
compress s@(_ :<| Seq.Empty) = s
compress (start@(Block _ (Full _)) :<| rest) = start :<| compress rest
compress (rest :|> end@(Block _ Null)) = compress rest :|> end
compress (Block sSize Null :<| (middle :|> Block eSize content@(Full _))) = (filledStart :<| compress (newStart <> middle <> newEnd)) :|> drainedEnd
  where transfer = min eSize sSize
        remStart = sSize - transfer
        remEnd = eSize - transfer
        filledStart = Block transfer content
        drainedEnd = Block transfer Null
        newStart
            | remStart == 0 = Seq.Empty
            | otherwise = Seq.singleton (Block remStart Null)
        newEnd
            | remEnd == 0 = Seq.Empty
            | otherwise = Seq.singleton (Block remEnd content)
compress _ = error "I should be covering all cases but hls wont shut up"

compress2 :: Storage -> Storage
compress2 Seq.Empty = Seq.Empty
compress2 s@(_ :<| Seq.Empty) = s
compress2 (start@(Block _ (Full _)) :<| rest) = start :<| compress2 rest
compress2 (start@(Block s Null) :<| rest) = (fromMaybe (start :<| compress2 rest') makeFit) <> space
    where canMove (Block _ Null) = False
          canMove (Block st (Full _)) = st <= s
          (space, rest') = Seq.spanr isEmpty rest
          fitsInd = Seq.findIndexR canMove rest'
          makeFit = do
               ind <- Seq.findIndexR canMove rest' 
               b@(Block s' (Full nr')) <- rest' Seq.!? ind
               let remStart = s - s'
                   newStart = (if s - s' > 0 then Seq.singleton $ Block (s - s') Null else Seq.Empty)
                   restDrained = Seq.update ind  (Block s' Null) rest'
               pure (b :<| (compress2 $ newStart <> restDrained))

asBytes :: Storage -> [Int]
asBytes = concatMap go
    where go (Block s Null) = replicate s 0
          go (Block s (Full nr)) = replicate s nr

checksum :: Storage -> Int
checksum = sum .zipWith (*) [0..] . asBytes

part1 = checksum . compress
part2 = checksum . compress2
--part2 = concatMap show . compress2

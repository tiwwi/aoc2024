module Helpers.Text (
    readT,
    readMaybeT,
    replaceAll,
    quickParseT
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Text.Read (readMaybe)

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

quickParseT :: T.Parser a -> T.Text -> a
quickParseT p txt = case T.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

replaceAll :: [(T.Text, T.Text)] -> T.Text -> T.Text
replaceAll pairs base = foldr go base pairs
    where go (a,b) t = T.replace a b t
    

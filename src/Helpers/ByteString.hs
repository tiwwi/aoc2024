module Helpers.ByteString (
    quickParseB
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as B

quickParseB :: B.Parser a -> B.ByteString -> a
quickParseB p txt = case B.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

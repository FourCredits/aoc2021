module Utils where

import Text.Parsec

num :: (Num a) => Parsec String () a
num = fromIntegral . read <$> many1 digit
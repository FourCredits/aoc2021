module Utils where

import Data.Either
import Text.Parsec

type Parser = Parsec String ()

doAParse :: Parser p -> p -> String -> p
doAParse p def = fromRight def . parse p ""

num :: (Num p) => Parser p
num = fromIntegral . read <$> many1 digit

sepByCommas :: Parser p -> Parser [p]
sepByCommas p = sepBy p (char ',')

sepEndByNewLines :: Parser p -> Parser [p]
sepEndByNewLines p = sepEndBy p endOfLine

sepByNewLines :: Parser p -> Parser [p]
sepByNewLines p = sepBy p endOfLine

sepPair :: Parser p -> Parser sep -> Parser (p, p)
sepPair p sep = (,) <$> (p <* sep) <*> p

module Utils.Parsing where

import Data.List
import Data.Bifunctor

splitOn :: String -> String -> [String]
splitOn sep = go
  where
    go s
      | null s = []
      | sep `isPrefixOf` s = go (drop l s)
      | Just index <- findIndex (sep `isPrefixOf`) $ tails s =
        uncurry (:) $ second (go . drop l) $ splitAt index s
      | otherwise = [s]
    l = length sep

twoOf :: [a] -> (a, a)
twoOf [x, y] = (x, y)
twoOf _ = error "must be exactly two things"

splitBy :: (String -> a) -> String -> String -> (a, a) 
splitBy f sep = twoOf . map f . splitOn sep

commas :: String -> [String]
commas = splitOn ","

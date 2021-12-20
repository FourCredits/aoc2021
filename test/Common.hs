module Common
  ( module Test.HUnit
  , realDeal
  ) where

import Test.HUnit

realDeal ::
     (Eq b, Show b, Eq c, Show c)
  => FilePath
  -> (String -> a)
  -> (a -> b, b)
  -> (a -> c, c)
  -> Test
realDeal file parser (p1, a1) (p2, a2) =
  "real deal" ~: do
    input <- parser <$> readFile file
    a1 @=? p1 input
    a2 @=? p2 input


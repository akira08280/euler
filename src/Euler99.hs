{--
  https://projecteuler.net/problem=99

  log a ^ x = p
  
  means that natural logarithm e will be raised to the power of p raised to (a ^ x).
  And, aboeve equation can be converted as below.

  x * log a = p

  In other words,instead of comparing the magnitude of a ^ x, 
  we compare by the magnitude of p.
--}

module Euler99 (e99_solve) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import System.IO (readFile)

e99_solve :: IO Int
e99_solve = do
  file <- readFile "src/resources/p099_base_exp.txt"
  let
    pairs  = map (\line -> read ("(" ++ line ++ ")") :: (Float, Float)) . lines $ file
    pairs' = map (\(base, exponent) -> exponent * log base) pairs
    max    = maximum pairs'
  return . fromJust . fmap succ . elemIndex max $ pairs'

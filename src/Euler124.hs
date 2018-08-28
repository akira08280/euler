{--
  https://projecteuler.net/problem=124
--}

module Euler124 (e124_solve) where

import Data.List (nub, sortBy)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)

e124_solve :: Integer
e124_solve =
  let
    rads = map rad [1..100000]
    sorted = sortBy (comparing snd) . zip [1..] $ rads
  in
    fst $ sorted !! 9999

rad :: Integer -> Integer
rad = product . nub . primeFactors

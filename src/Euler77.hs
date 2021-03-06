{--
  https://projecteuler.net/problem=77
--}

module Euler77 (e77Solve) where

import Data.Numbers.Primes (primes)

e77Solve :: Int
e77Solve = succ . length . takeWhile (<= 5 * 10 ^ 3) . map sumofways $ [1..]

sumofways :: Integral a => Int -> a
sumofways n = let ps = takeWhile (<= n) primes in ways ps !! n
  where
    ways [] = 1 : repeat 0
    ways (coin:coins) = n 
      where
        n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

{--
  https://projecteuler.net/problem=35
--}

module Euler35 (e35Solve) where

import Text.Regex.Posix ((=~))
import Data.Numbers.Primes (isPrime, primes)
import Common (digit)

e35Solve :: Int
e35Solve =
  let
    ps  = filter ((=~ "^[1|3|7|9]+$") . show) $ takeWhile (< 10 ^ 6) primes
    cps = length . filter (== True) . map (allPrimes . circle) $ ps
  in
    cps + 2 -- add 2, 5 primes

allPrimes :: Integral a => [a] -> Bool
allPrimes = all isPrime

circle :: Int -> [Int]
circle n = scanl circle' n d
  where
    d = replicate (pred . digit $ n) (pred . digit $ n)
    circle' :: Int -> Int -> Int
    circle' acc x = r
      where
        a = divMod acc 10
        r = snd a * 10 ^ x + fst a

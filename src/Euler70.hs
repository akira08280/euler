{--
  https://projecteuler.net/problem=70

  n / phy(n) = 1 % ((1 - 1 % p1) * (1 - 1 % p2) * (1 - 1 % p3) - (1 - 1 % p4)) ...

  Since phy(n) < n, we know that n have to be close to phy(n) to make n / phy(n) as small as possible.
  The more prime factor's of n, n / phy(n) will be large.
  Ideally, it will be the smallest when n is prime, but those are impossible to be permutations of each other.
  So next, it is that next choice is composite two distinct primes composite.
  --
  phy(p1p2) = p1p2(1 - 1 % p1)(1 - 1 % p2) = (p1 - 1)(p2 - 1) = p1p2 - p1 - p2 + 1

  Integer a is to be permutation of integer b, their difference must be a multiple of 9.
  So, n - phy(n) is a multiple of 9.
  As became clear above, n - phy(n) = p1p2 - (p1p2 - p1 - p2 + 1) = p1 + p2 - 1.
  That's means (p1 + p2) % 9 must be equal 1.
  --
  d1,d2,d3,d4 -> d2,d3,d4,d1 (Whichever is larger, generally is not impaired.)
  (d1 * 1000 + d2 * 100 * d3 * 10 + d4 * 1) - (d2 * 1000 + d3 * 100 + d4 * 10 + d1 * 1) = d1(1000-1) + d2(100-1000) + d3(10-100) + d4(1-10)
                                                                                        = d1 * 999 - d2 * 990 - d3 * 90 - d4 * 99
                                                                                        = 9(d1 * 11 - d2 * 110 - d2 * 10 - d4 * 11)

  And, p1 is need to be as close as possible to p2 to find minimum of n / phy(n).
  --
  [p1 = 2,p2 = 4999999] phy(p1p2) = (2-1)(4999999-1) = 4999998. p1p2 % phy(p1p2) = 9999998 % 4999998. (2.00000040000016)
  [p1 = 3,p2 = 3333331] phy(p1p2) = (3-1)(3333331-1) = 6666660. p1p2 % phy(p1p2) = 9999993 % 6666660. (1.50000045000045)
  [p1 = 5,p2 = 1999992] phy(p1p2) = (5-1)(1999993-1) = 7999968. p1p2 % phy(p1p2) = 9999965 % 7999968. (1.2500006250025)
  [p1 = 7,p2 = 1428571] phy(p1p2) = (7-1)(1428571-1) = 8571240. p1p2 % phy(p1p2) = 9999997 % 8571240. (1.1666919838903123)
  [p1 = 3137,p2 = 3121] phy(p1p2) = (3137-1)(3121-1) = 9784320. p1p2 % phy(p1p2) = 9790577 % 9784320. (1.0006394925758766)

  At last, both n and phy(n) is need to be close to 10000000.
  --
  10       / 9      = 1.11111111111111
  10000000 / 999999 = 1.00000010000001
--}

module Euler70 (e70_solve) where

import Control.Monad (guard)
import Data.Ratio ((%), Ratio)
import Data.Ord (comparing)
import Data.List ((\\), minimumBy)
import Data.Numbers.Primes (primes)

e70_solve :: Integer
e70_solve =
  let
    n = 10 ^ 7
  in
    fst . minimumBy (comparing snd) . phyPerm $ n

phyPerm :: Integer -> [(Integer, Ratio Integer)]
phyPerm n = do
  p1 <- target n
  p2 <- dropWhile (<= p1) $ target n
  guard (p1 * p2 < n)
  guard ((p1 + p2) `mod` 9 == 1)
  guard (isPerm (p1 * p2) ((p1 - 1) * (p2 - 1)))
  return (p1 * p2, (p1 * p2) % ((p1 - 1) * (p2 - 1)))
 
target :: Integral a => a -> [a]
target n = takeWhile (<= upper) . dropWhile (<= lower) $ primes
  where
    n' = floor . sqrt . fromIntegral $ n
    upper = n' * 2
    lower = n' `div` 2

isPerm :: Integer -> Integer -> Bool
isPerm a b = null $ show a \\ show b

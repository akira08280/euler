{--
  https://wiki.haskell.org/Euler_problems/51_to_60#Problem_60

  Prime except 3 is congruent to 1 or 2 modulo 3.
  If mod 1 primes is combined to mod 2 primes, it's divided by 3.
  So, we can combine mod 1 with mod 1, or mod 2 with mod 2.

	$ time ./60
	
	26033

	real	0m0.473s
	user	0m0.470s
	sys	0m0.000s
--}

module WikiEuler60 (wiki_e60_solve) where

import Data.Char (digitToInt)
import MillerRabin (millerRabinPrimality)

wiki_e60_solve :: Integer
wiki_e60_solve = sum . head . solve . primesOfMod $ 1

solve :: [Integer] -> [[Integer]]
solve ps = do
  a <- ps
  let m = f a $ dropWhile (<= a) ps
  b <- m
  let n = f b $ dropWhile (<= b) m
  c <- n
  let o = f c $ dropWhile (<= c) n
  d <- o
  let p = f d $ dropWhile (<= d) o
  e <- p
  return [a, b, c, d, e]
  where
    f x = filter (\y -> isPrime (read $ shows x $ show y) && isPrime (read $ shows y $ show x))

isPrime :: Integer -> Bool
isPrime x
  | x == 3 = True
  | otherwise = and [millerRabinPrimality x n | n <- [2,3]]

primesOfMod :: Int -> [Integer]
primesOfMod m = 3 : filter (\p -> isPrime p && digitSum p `mod` 3 == m) [5,7..9999]

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

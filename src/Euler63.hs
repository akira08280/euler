{--
  https://projecteuler.net/problem=63

  n's range is 1 <= n <= 9.
  if n is greater equal than 10, 10 ^ p consists of  p+1 digits.
--}

module Euler63 (e63_solve) where

e63_solve :: Int
e63_solve = length nthPower

nthPower :: [(Integer, Int, Integer)]
nthPower = do
  n <- [1..9]
  p <- takeWhile (\p -> (== p) . digit . (n ^) $ p) [1..]
  return (n, p, n ^ p)

digit :: Integer -> Int
digit = length . show

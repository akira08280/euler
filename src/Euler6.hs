{--
  https://projecteuler.net/problem=6
--}

module Euler6 (e6_solve) where

e6_solve :: Integral a => a
e6_solve = round $ sumOfSquares limit ^ 2 - squareOfSums limit

sumOfSquares :: Fractional a => a -> a
sumOfSquares n = n * (n + 1) / 2

squareOfSums :: Fractional a => a -> a
squareOfSums n = (n * (n + 1) * (2 * n + 1)) / 6

limit :: Double
limit = 100

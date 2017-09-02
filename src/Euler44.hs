{--
  https://projecteuler.net/problem=44

  n = ((√24 x + 1) + 1) / 6 must consist of integer.
--}

module Euler44 (e44_solve) where

import Control.Monad (guard)

e44_solve :: Integral a => a
e44_solve = head pentagonals

pentagonals :: Integral a => [a]
pentagonals = do
  a <- [1..]
  b <- [a-1,a-2..1]
  let
    p1 = nthPentagonal a
    p2 = nthPentagonal b
  guard (isPentagonal (p1 - p2) && isPentagonal (p1 + p2))
  return (p1 - p2)

nthPentagonal :: Integral a => (a -> a)
nthPentagonal = (\n -> fst $ divMod (n * (3 * n - 1)) 2)

isPentagonal :: Integral a => a -> Bool
isPentagonal x = n == n'
  where
    n = (sqrt (1 + 24 * (fromIntegral x)) + 1) / 6
    n' = fromIntegral . floor $ n

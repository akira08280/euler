{--
  https://projecteuler.net/problem=45

  Hexagonal numbers is subset of Triangular numbers.
  
  Triangular numbers
  n (n + 1) / 2
  n = 2 * m - 1 (odd)
  
  (2m - 1) (2m - 1 + 1) / 2 = (4m ^ 2 - 2m) / 2
                            = (2m ^ 2 - m)
                            = m (2m - 1)
                            = Hexagonal numbers
  
  So Hexagonal numbers is odd Triangular numbers, we consider of Hexagonal numbers and Pentagonal numbers.
--}

module Euler45 (e45_solve) where

e45_solve :: Integral a => a
e45_solve = head . filter isPentagonal . map nthHexagonal $ [144..]

nthHexagonal :: Integral a => (a -> a)
nthHexagonal n = n * (2 * n - 1)

isPentagonal :: Integral a => a -> Bool
isPentagonal x = n == n'
  where
    n = (sqrt (1 + 24 * fromIntegral x) + 1) / 6
    n' = fromIntegral . floor $ n

{--
  https://projecteuler.net/problem=116
--}

module Euler116 (e116_solve) where

import Block (fixBlockWays)

e116_solve :: Integer
e116_solve = sum . map (pred . last . flip fixBlockWays 50) $ [2..4]

{--
  https://projecteuler.net/problem=116
--}

module Euler116 (e116Solve) where

import Block (fixBlockWays)

e116Solve :: Integer
e116Solve = sum . map (pred . last . flip fixBlockWays 50) $ [2..4]

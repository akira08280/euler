{--
  https://projecteuler.net/problem=493
--}

module Euler493 (e493Solve) where

import Common (c, decimalRound)

e493Solve :: Double
e493Solve = decimalRound (7 * (1 - (fromIntegral . c 60 $ 20) / (fromIntegral . c 70 $ 20))) 9

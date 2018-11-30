{--
  https://projecteuler.net/problem=493
--}

module Euler493 (e493Solve) where

import Common (c)
import Text.Printf (printf, PrintfType)

e493Solve :: PrintfType t => t
e493Solve = printf "%.9f\n" (7 * (1 - (fromIntegral . c 60 $ 20) / (fromIntegral . c 70 $ 20)) :: Double)

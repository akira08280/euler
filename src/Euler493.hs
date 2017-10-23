{--
  https://projecteuler.net/problem=493
--}

module Euler493 (e493_solve) where

import Common (c)
import Text.Printf (printf, PrintfType)

e493_solve :: PrintfType t => t
e493_solve = printf "%.9f\n" (7 * (1 - (fromIntegral . c 60 $ 20) / (fromIntegral . c 70 $ 20)) :: Double)

{--
  https://projecteuler.net/problem=103
--}

module Euler103 (e103_solve) where

import Common (concatIntArray)
import SpecialSubset (isSpecialSubset)

e103_solve :: Int
e103_solve = concatIntArray . head . candidate $ [20, 31, 38, 39, 40, 44, 46]

candidate :: Integral a => [a] -> [[a]]
candidate = filter isSpecialSubset . mapM (\x -> [x-2..x+2])

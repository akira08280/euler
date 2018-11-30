{--
  https://projecteuler.net/problem=103
--}

module Euler103 (e103Solve) where

import Common (concatIntArray)
import SpecialSubset (isSpecialSubset)

e103Solve :: Int
e103Solve = concatIntArray . head . candidate $ [20, 31, 38, 39, 40, 44, 46]

candidate :: Integral a => [a] -> [[a]]
candidate = filter isSpecialSubset . mapM (\x -> [x-2..x+2])

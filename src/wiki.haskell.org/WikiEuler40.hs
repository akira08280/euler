{--
  https://wiki.haskell.org/Euler_problems/31_to_40#Problem_40
--}

module WikiEuler40 (wiki_e40_solve) where

import Data.Char (digitToInt)

wiki_e40_solve = d 1 * d 10 * d 100 * d 1000 * d 10000 * d 100000 * d 1000000
  where
    n = concat [show n | n <- [1..]]
    d j = digitToInt (n !! (j - 1))

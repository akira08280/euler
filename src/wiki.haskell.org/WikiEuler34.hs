{--
  https://wiki.haskell.org/Euler_problems/31_to_40#Problem_34
--}

module WikiEuler34 (wiki_e34_solve) where

import Data.Char

wiki_e34_solve = sum [ x | x <- [3..100000], x == facsum x ]
  where facsum = sum . map (product . enumFromTo 1 . digitToInt) . show

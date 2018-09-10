{--
  https://wiki.haskell.org/Euler_problems/1_to_10#Problem_1
--}

module WikiEuler1 (wiki_e1_solve) where

import Data.List (union)

wiki_e1_solve :: Integral a => a
wiki_e1_solve = sum ([3,6..999] `union` [5,10..999])

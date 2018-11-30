{--
  https://wiki.haskell.org/Euler_problems/1_to_10#Problem_1
--}

module WikiEuler1 (wikiE1Solve) where

import Data.List (union)

wikiE1Solve :: Integral a => a
wikiE1Solve = sum ([3,6..999] `union` [5,10..999])

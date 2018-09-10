{--
  https://wiki.haskell.org/Euler_problems/21_to_30#Problem_21
--}

module WikiEuler21 (wiki_e21_solve) where

import Data.Array

max_ = 100000

gen 100001 = []
gen n = [(i*n,n)|i <- [2 .. max_ `div` n]] ++ gen (n + 1)

arr = accumArray (+) 0 (0, max_) (gen 1)

wiki_e21_solve = sum $ filter (\a -> let b = (arr ! a) in b /= a && (arr ! b) == a) [1..(10000 - 1)]

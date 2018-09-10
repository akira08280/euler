{--
  https://wiki.haskell.org/Euler_problems/51_to_60#Problem_52
--}

module WikiEuler52 (wiki_e52_solve) where

import Data.List
 
hasSameDigits a b = null (show a \\ show b)
  
check n = all (hasSameDigits n) (map (n*) [2..6])
   
wiki_e52_solve = head $ filter check [1..]

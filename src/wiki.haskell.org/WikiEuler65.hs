{--
  https://wiki.haskell.org/Euler_problems/61_to_70#Problem_65

  fraction e = 2%1 +                1
                     -------------------------------
                     1%1 +             1
                           -------------------------
                           2%1 +          1
                                 -------------------
                                 1%1 +       1
                                       -------------
                                       1%1 +    1
                                             -------
                                                4

  (/) can get inverse in context of Data.Ratio.
  
  1 / 4     = 0.25
  1 / (4%1) = 1 % 4
--}

module WikiEuler65 (wiki_e65_solve) where

import Data.Char
import Data.Ratio
 
e = 2 : concat [ [1, 2*i, 1] | i <- [1..] ]

fraction [x] = x % 1
fraction (x:xs) = x % 1 + 1 / fraction xs

wiki_e65_solve = sum $ map digitToInt $ show $ numerator $ fraction $ take 100 e

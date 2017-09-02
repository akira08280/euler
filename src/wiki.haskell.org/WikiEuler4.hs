{--
  https://wiki.haskell.org/Euler_problems/1_to_10#Problem_4
--}

module WikiEuler4 (wiki_e4_solve) where

import Data.List (union)

wiki_e4_solve =
  maximum [x | y<-[100..999], z<-[y..999], let x=y*z, let s=show x, s==reverse s]

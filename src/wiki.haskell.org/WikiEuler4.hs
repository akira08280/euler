{--
  https://wiki.haskell.org/Euler_problems/1_to_10#Problem_4
--}

module WikiEuler4 (wikiE4Solve) where

import Data.List (union)

wikiE4Solve =
  maximum [x | y<-[100..999], z<-[y..999], let x=y*z, let s=show x, s==reverse s]

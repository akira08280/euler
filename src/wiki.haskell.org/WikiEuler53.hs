{--
  https://wiki.haskell.org/Euler_problems/51_to_60#Problem_53
--}

module WikiEuler53 (wiki_e53_solve) where

facs = scanl (*) 1 [1..100]
comb (r,n) = facs!!n `div` (facs!!r * facs!!(n-r))
perms = [(n,x) | x<-[1..100], n<-[1..x]]
wiki_e53_solve = length $ filter (>1000000) $ map comb $ perms

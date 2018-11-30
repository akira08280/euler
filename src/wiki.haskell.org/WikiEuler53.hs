{--
  https://wiki.haskell.org/Euler_problems/51_to_60#Problem_53
--}

module WikiEuler53 (wikiE53Solve) where

facs = scanl (*) 1 [1..100]
comb (r, n) = facs !! n `div` (facs !! r * facs !! (n - r))
perms = [(n, x) | x <- [1..100], n <- [1..x]]
wikiE53Solve = length $ filter (> 1000000) $ map comb perms

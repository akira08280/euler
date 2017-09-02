{--
  https://wiki.haskell.org/Euler_problems/91_to_100#Problem_93
--}

module WikiEuler93 (wiki_e93_solve) where

import Data.List
import Control.Monad
import Data.Ord (comparing)

solve [] [x] = [x]
solve ns stack = 
  pushes ++ ops
  where
  pushes = do
    x <- ns
    solve (x `delete` ns) (x:stack)
  ops = do
    guard (length stack > 1)
    x <- opResults (stack!!0) (stack!!1)
    solve ns (x : drop 2 stack)

opResults a b = 
  [a*b,a+b,a-b] ++ (if b /= 0 then [a / b] else [])

results xs = fun 1 ys
  where
  ys = nub $ sort $ map truncate $ filter (\x -> x > 0 && floor x == ceiling x) $ solve xs [] 
  fun n (x:xs) 
    |n == x =fun (n+1) xs 
    |otherwise=n-1

cmp = comparing results

wiki_e93_solve = maximumBy cmp $ [[a,b,c,d] | a <- [1..10], 
                                              b <- [a+1..10], 
                                              c <- [b+1..10], 
                                              d <- [c+1..10]]

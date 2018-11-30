{--
  https://wiki.haskell.org/Euler_problems/91_to_100#Problem_93
--}

module WikiEuler93 (wikiE93Solve) where

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
      x <- opResults (head stack) (stack !! 1)
      solve ns (x : drop 2 stack)

opResults a b = [a * b, a + b, a - b] ++ [a / b | b /= 0]

results xs = fun 1 ys
  where
  ys = nub $ sort $ map truncate $ filter (\x -> x > 0 && floor x == ceiling x) $ solve xs [] 
  fun n (x:xs) 
    | n == x = fun (n + 1) xs
    | otherwise = n - 1

cmp = comparing results

wikiE93Solve = maximumBy cmp [[a,b,c,d] | a <- [1..10],
                                            b <- [a + 1..10],
                                            c <- [b + 1..10],
                                            d <- [c + 1..10]]

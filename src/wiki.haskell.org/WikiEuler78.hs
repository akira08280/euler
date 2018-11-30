{--
  https://wiki.haskell.org/Euler_problems/71_to_80#Problem_78

  Wikipedia
  https://en.wikipedia.org/wiki/Partition_(number_theory)

  I don't understand this theory perfectly.
--}

module WikiEuler78 (wikiE78Solve) where

import Data.Array

partitions :: Array Int Integer
partitions = 
  array (0,1000000) $ 
  (0,1) : 
  [(n,sum [s * partitions ! p|
  (s,p) <- zip signs $ parts n])|
  n <- [1..1000000]]
  where
    signs = cycle [1, 1, -1, -1]
    suite = map penta $ concat [[n, -n] | n <- [1..]]
    penta n = n * (3 * n - 1) `div` 2
    parts n = takeWhile (>= 0) [n - x | x <- suite]

wikiE78Solve :: Int
wikiE78Solve = head $ filter (\x -> (partitions ! x) `mod` 1000000 == 0) [1..]

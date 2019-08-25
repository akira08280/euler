{--
  https://projecteuler.net/problem=500
--}

module Euler500 (e500Solve) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Numbers.Primes (primes)

e500Solve :: Integral a => a
e500Solve = foldr1 mod500500507 . take 500500 . sortTable $ primeTable

mod500500507 :: Integral a => a -> a -> a
mod500500507 a b = a * b `mod` 500500507

primeTable :: Integral a => [[a]]
primeTable = do
  power <- map (2 ^) [0..]
  return $ map (^ power) primes

sortTable :: Ord a => [[a]] -> [a]
sortTable = sortTable' 1
  where
    sortTable' k pss = min:sortTable' m generated
      where
        -- Add 1 to get the next row for comparison.
        (i, min) = minIndex . extract (succ k) $ pss
        {--
         - The value(i) returned by "minIndex" starts at 0 because of the array index.
         - Thus it is necessary to add 1 to match the argument of "take."
        --}
        m = max k (succ i)
        generated = generate min pss

extract :: Ord a => Int -> [[a]] -> [a]
extract k = map head . take k

minIndex :: Ord a => [a] -> (Int, a)
minIndex xs = (index, min)
  where
    min = minimum xs
    index = fromJust . elemIndex min $ xs

generate :: Ord a => a -> [[a]] -> [[a]]
generate m ((x:xs):yss)
  | x == m = xs:yss
  | otherwise = (x:xs):generate m yss

{--
  https://projecteuler.net/problem=500
--}

module Euler500 (e500Solve) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Numbers.Primes (primes)

e500Solve :: Integral a => a
e500Solve = foldr1 mod500500507 . take 500500 . merge $ primeTable

mod500500507 :: Integral t => t -> t -> t
mod500500507 a b = a * b `mod` 500500507

primeTable :: Integral t => [[t]]
primeTable = do
  power <- map (2 ^) [0..]
  return $ map (^ power) primes

merge :: Ord a => [[a]] -> [a]
merge = merge' 1
  where
    merge' k pss = min:merge' m generated
      where
        (i, min) = minIndex . extract (succ k) $ pss
        m = max k (succ i)
        generated = generate min pss

extract :: Ord t => Int -> [[t]] -> [t]
extract k = map head . take k

minIndex :: Ord t => [t] -> (Int, t)
minIndex xs = (index, min)
  where
    min = minimum xs
    index = fromJust . elemIndex min $ xs

generate :: Ord t => t -> [[t]] -> [[t]]
generate m (xxs@(x:xs):yss)
  | x == m = xs:yss
  | otherwise = xxs:generate m yss

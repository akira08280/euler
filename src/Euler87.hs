{--
  https://projecteuler.net/problem=87
--}

module Euler87 (e87_solve) where

import Control.Monad (guard)
import Data.Numbers.Primes (primes)
import qualified Data.Set as Set (fromList, size)

e87_solve :: Int
e87_solve =
  let
    limit = 5 * 10 ^ 7
  in
    Set.size . Set.fromList . powers $ limit

powers :: Integral a => a -> [a]
powers limit = do
  let
    a = ceiling $ (fromIntegral limit) ** (1/2)
    b = ceiling $ (fromIntegral limit) ** (1/3)
    c = ceiling $ (fromIntegral limit) ** (1/4)
  p2 <- takeWhile (< a) primes
  p3 <- takeWhile (< b) primes
  p4 <- takeWhile (< c) primes
  let
    s = p2 ^ 2 + p3 ^ 3 + p4 ^ 4
  guard (s < limit)
  return s

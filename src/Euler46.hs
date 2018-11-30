{--
  https://projecteuler.net/problem=46
--}

module Euler46 (e46Solve) where

import Control.Monad (guard)
import Data.Numbers.Primes (primes)

e46Solve :: Integral a => a
e46Solve = head composites

composites :: Integral a => [a]
composites = do
  a <- [3,5..]
  let
    primes' = takeWhile (<= a) primes
  guard (all (not . isTwiceSquare . (+ a) . negate) primes')
  return a

isTwiceSquare :: Integral a => a -> Bool
isTwiceSquare n = floor n' == ceiling n' 
  where
    n' = sqrt (fromIntegral . div n $ 2)

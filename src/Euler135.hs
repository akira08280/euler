{--
  https://projecteuler.net/problem=135

  (x + k) ^ 2 - x ^ 2 - (x - k) ^ 2 = x * (4 * k - x) = n
--}

module Euler135 (e135Solve) where

import Control.Monad (guard)
import qualified Data.Map as Map (fromListWith, filter, size)

e135Solve :: Int
e135Solve = Map.size . Map.filter (== 10) . Map.fromListWith (+) . findAllN $ limit

findAllN :: (Num b, Integral t) => t -> [(t, b)]
findAllN lim = concat $ do
  x <- [1..lim]
  return . takeWhile ((< lim) . fst) $ findNs x
  where
    findNs x = do
      let
        lower = 1 + (x `div` 4) -- Since (4 * k - x) > 0, so k > x / 4
        upper = x - 1           -- Since (x - k) > 0, so x > k
      k <- [lower..upper]
      let
        n = x * (4 * k - x)
      return (n, 1)

limit :: Integral a => a
limit = 10 ^ 6

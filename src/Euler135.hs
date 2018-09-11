{--
  https://projecteuler.net/problem=135
--}

module Euler135 (e135_solve) where

import Control.Monad (guard)
import qualified Data.Map as Map (fromListWith, filter, size)

e135_solve :: Int
e135_solve = Map.size . Map.filter (== 10) . Map.fromListWith (+) . findNs $ limit

findNs :: (Num b, Integral t) => t -> [(t, b)]
findNs lim = concat $ do
  x <- [1..lim]
  return . takeWhile ((< lim) . fst) $ solveN x
  where
    solveN x = do
      let
        lower = 1 + (x `div` 4)
        upper = x - 1
      k <- [lower..upper]
      let
        n = x * (4 * k - x)
      return (n, 1)

limit :: Integral a => a
limit = 10 ^ 6

{--
  https://projecteuler.net/problem=29
--}

module Euler29 (e29_solve) where

import qualified Data.Set as Set (fromList)

e29_solve :: Int
e29_solve = length . Set.fromList $ (\a b -> a ^ b) <$> [2..100] <*> [2..100]

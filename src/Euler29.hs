{--
  https://projecteuler.net/problem=29
--}

module Euler29 (e29Solve) where

import qualified Data.Set as Set (fromList)

e29Solve :: Int
e29Solve = length . Set.fromList $ (^) <$> [2..100] <*> [2..100]

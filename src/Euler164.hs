{--
  https://projecteuler.net/problem=164
--}

module Euler164 (e164Solve, f) where

import qualified Data.Map as M
import Data.Map.Internal
import Data.Maybe

e164Solve :: Integer
e164Solve = sum $ do
  x <- [1..9]
  return $ f (19, x, 0)

memo :: Map (Integer, Integer, Integer) Integer
memo = M.fromList $ do
  digits <- [0..20]
  prev <- [0..9]
  prevprev <- [0..9]
  let
    ways = f (digits, prev, prevprev)
  return ((digits, prev, prevprev), ways)

f' :: (Integer, Integer, Integer) -> Integer
f' t = fromJust (M.lookup t memo)

f :: (Integer, Integer, Integer) -> Integer
f (0, prev, prevprev) = 1
f (n, prev, prevprev) = sum $ do
  x <- [0..9 - (prev + prevprev)]
  return $ f' (n - 1, x, prev)

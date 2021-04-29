{--
  https://projecteuler.net/problem=164
--}

module Euler164 (e164Solve) where

import qualified Data.Map as M
import Data.Maybe

e164Solve = sum $ [ f (19, x, 0) | x <- [1..9] ]

m = M.fromList [((digits, prev, prevprev), ways) | digits <- [0..20],
                                                   prev <- [0..9],
                                                   prevprev <- [0..9],
                                                   let ways = f (digits, prev, prevprev) ]

f' :: (Integer, Integer, Integer) -> Integer
f' t = fromJust (M.lookup t m)

f (0, prev, prevprev) = 1
f (n, prev, prevprev) = sum [ f' ((n-1), x, prev) | x <- [0..9 - (prev + prevprev)] ]

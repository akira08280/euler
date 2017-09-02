{--
  https://projecteuler.net/problem=31
--}

module Euler31 (e31_solve) where

import Control.Monad (forM_)
import Data.Array.ST (newArray, runSTUArray)
import Data.Array.Base(unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray, elems)

e31_solve :: Int
e31_solve = last . elems $ dp

dp :: UArray Int Int
dp = runSTUArray $ do
  let
    target = 200
    coins = [1,2,5,10,20,50,100,200]
  ways <- newArray (0, target) 0
  unsafeWrite ways 0 1
  forM_ coins $ \coin -> do
    mapM_ (\j -> unsafeRead ways j >>= (\k -> unsafeRead ways (j-coin) >>= (\l -> unsafeWrite ways j (l+k)))) [coin..target]
  return ways

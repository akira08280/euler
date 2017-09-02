{--
  https://projecteuler.net/problem=14
--}

module Euler14 (e14_solve) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Base (unsafeRead, unsafeWrite)

e14_solve :: Int
e14_solve = maxCollatzLength (10 ^ 6)

-- If the number in process is less than first number, getting the rest length from cache.
maxCollatzLength :: Int -> Int
maxCollatzLength n = runST $ do
  maxRef <- newSTRef 0
  numRef <- newSTRef 0
  cache <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  unsafeWrite cache 1 1
  forM_ [1..n] $ \i -> do
    let
      (j, head) = collatz i
    tail <- unsafeRead cache j
    let
      len = tail + head
    unsafeWrite cache i len
    readSTRef maxRef >>= (\max -> when (len > max) $ writeSTRef maxRef len >> writeSTRef numRef i)
  readSTRef numRef
  
collatz :: Integral a => a -> (a, a)
collatz n = collatz' n n 0
  where
    collatz' :: Integral a => a -> a -> a -> (a, a)
    collatz' x y z
      | x < y || x == 1 = (x, z)
      | even x = collatz' (x `div` 2) y $ succ z
      | odd x = collatz' (succ (x * 3)) y $ succ z
      | otherwise = error "a cold day in hell"

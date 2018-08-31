{--
  Rads module for euler 124, 127
--}

module Rads where

import Data.List (nub)
import Data.Numbers.Primes (primeFactors)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as U (Vector, thaw, fromListN, unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as UM (unsafeRead, unsafeWrite)

list :: Int -> [(Int, Int)]
list limit = (0,0) : (zip [1..] $ map rad [1..limit])
  where
    rad = product . nub . primeFactors

sieve :: Int -> U.Vector (Int, Int)
sieve limit = runST $ do
  radsVec <- U.thaw $ U.fromListN (succ limit) $ zip [0..] (0:[1,1..])
  forM_ [2..limit] $ \n -> do
    rad <- UM.unsafeRead radsVec n
    when (snd rad == 1) $ do
      forM_ [n,n+n..limit] $ \j -> do
        UM.unsafeRead radsVec j >>= (\t -> let radt = snd t in UM.unsafeWrite radsVec j (j, radt * n))
  U.unsafeFreeze radsVec

{--

This function causes segmentation fault at over 10 ^ 4.

sieve :: Int -> V.Vector (Int, Int)
sieve limit =
  let
    vec = V.fromList $ zip [0..limit] (0:[1,1..])
  in
    foldl (\v i -> sieve i v) vec [2..limit-1]
  where
    sieve n vec'
      | rad > 1 = vec'
      | otherwise =
        let
          length = V.length vec'
          indices = [n,n+n..length-1]
          rads = map (\i -> let rad = snd $ vec' V.! i in (i, (i, n * rad))) indices
        in
          vec' V.// rads
        where
          rad = snd $ vec' V.! n
--}

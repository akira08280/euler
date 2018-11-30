{--
  https://projecteuler.net/problem=95

  This code was referred to the forum.I appreciate daniel.is.fischer's wonderful code.
  I learned that you can handle set and array in functions defined by ST monad.

  ghc -O2 --make 95.hs
--}

{-# LANGUAGE FlexibleContexts #-}

module Euler95 (e95Solve) where

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (when, unless, forM_)
import qualified Data.IntSet as Set
import System.Environment (getArgs)

findMinMax :: Int -> (Int,Int)
findMinMax lim = runST $ do
    -- first get the sum of prime factor of each number
    arr <- newArray (0,lim) 1 :: ST s (STUArray s Int Int)
    forM_ [2 .. lim `div` 2] $ \i ->
      mapM_ (\j -> unsafeRead arr j >>= (\k -> unsafeWrite arr j (i + k))) [2 * i, (2 * i) + i .. lim]
    -- now find the longest chain
    let follow n i chain
            | i == n    = return (Set.size chain)
            | i < n     = return 0
            | lim < i   = return 0
            | i `Set.member` chain = return 0
            | otherwise = do
                next <- unsafeRead arr i
                follow n next (Set.insert i chain)
        chainLength i = do
            next <- unsafeRead arr i
            if next < i
                then return 0
                else follow i next (Set.singleton i)
        find mm len i
            | lim < i   = return (mm,len)
            | otherwise = do
                cl <- chainLength i
                if len < cl
                    then find i cl (i+1)
                    else find mm len (i+1)
    find 6 1 7

e95Solve :: IO Int
e95Solve = do
    args <- getArgs
    let lim = case args of
                (a:_) -> read a
                _ -> 10 ^ 6
    return . fst . findMinMax $ lim

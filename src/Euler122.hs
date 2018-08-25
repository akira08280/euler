{--
  https://projecteuler.net/problem=122

  I implemented it with reference to this URL.
  Since Data.Array.Diff is deprecated, it is corrected to Data.Vector.

  [URL]
  https://wiki.haskell.org/Euler_problems/121_to_130#Problem_122
--}

module Euler122 (e122_solve) where

import Data.List (nub)
import Control.Monad (liftM2)
import qualified Data.Vector as V ((!), (//), Vector, sum, fromListN) 

e122_solve :: Int
e122_solve = V.sum . depthAddChain 2 baseBranch $ baseMins

depthAddChain :: (Num a, Ord a, Enum a) => a -> [Int] -> V.Vector a -> V.Vector a
depthAddChain 12 branch mins = mins
depthAddChain  d branch mins = foldl step mins nextExps
  where
    nextExps = nub . filter (> head branch) . liftM2 (+) branch $ branch
    step da e
      | i >= maxExp = da
      | otherwise = case compare (da V.! i) d of
                      GT -> depthAddChain (succ d) (e:branch) $ da V.// [(i,d)]
                      EQ -> depthAddChain (succ d) (e:branch) da
                      LT -> da
      where
        i = pred e

baseBranch :: [Int]
baseBranch = [1,2]

baseMins :: V.Vector Int
baseMins = V.fromListN maxExp $ 0:1:repeat maxBound

maxExp :: Int
maxExp = 200

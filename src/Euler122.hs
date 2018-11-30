{--
  https://projecteuler.net/problem=122

  I implemented it with reference to this URL.
  Since Data.Array.Diff is deprecated, it is corrected to Data.Vector.

  [URL]
  https://wiki.haskell.org/Euler_problems/121_to_130#Problem_122
--}

module Euler122 (e122Solve) where

import Data.List (nub)
import Control.Monad (liftM2)
import qualified Data.Vector as V ((!), (//), Vector, sum, fromListN) 

e122Solve :: Int
e122Solve = V.sum . depthAddChain 2 baseBranch $ baseMins

depthAddChain :: (Num a, Ord a, Enum a) => a -> [Int] -> V.Vector a -> V.Vector a
depthAddChain 12 branch mins = mins
depthAddChain  d branch mins = foldl step mins $ nextExps branch
  where
    step da e
      | e >= maxExp = da
      | otherwise = case compare (da V.! e) d of
                      GT -> depthAddChain (succ d) (e:branch) $ da V.// [(e,d)]
                      EQ -> depthAddChain (succ d) (e:branch) da
                      LT -> da

nextExps :: [Int] -> [Int]
nextExps current = nub . filter (> head current) . liftM2 (+) current $ current

baseBranch :: [Int]
baseBranch = [2,1]

-- Index of Data.Array.Diff starts from 1, but Vector's index starts from 0
baseMins :: V.Vector Int
baseMins = V.fromListN maxExp $ 0:0:1:repeat maxBound

maxExp :: Int
maxExp = succ 200

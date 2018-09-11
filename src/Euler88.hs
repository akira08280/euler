{--
  https://projecteuler.net/problem=88

  k = 2: 4  = 2 * 2                 = 2 + 2
  k = 3: 6  = 1 * 2 * 3             = 1 + 2 + 3
  k = 4: 8  = 1 * 1 * 2 * 4         = 1 + 1 + 2 + 4
  k = 5: 8  = 1 * 1 * 2 * 2 * 2     = 1 + 1 + 2 + 2 + 2
  k = 6: 12 = 1 * 1 * 1 * 1 * 2 * 6 = 1 + 1 + 1 + 1 + 2 + 6

  I referred to the euler forum expression.

  For a set of size k, N > k, as N=k would only happen if the sum were k ones. However, the product of this would be 1.

  Also we note that N = 2k is a guaranteed solution.
  The sum of these factors is 2+k, then a string of (k-2) ones will make a sum of 2k, and obviously the product will be 2k.

  Hence, M, the minimal sum-product made up of k elements, k < M <= 2k.

  As the maximum k was 12000, all of the solutions will be  found at or below 24000.
  However, my method then was to turn the problem on its head: rather than find all the factors of a given N,
  I found all the combinations of factors and calculated the number of elements to make that N.

  Starting with 2*2, my algorithm went through 2 * 3, 2 * 4, 2 * 5, ..., 2 * 12000;
  it stops here because we've reached  the maximum product: 24000.

  Then 3 * 3, 3 * 4, 3 * 5, ..., 3 * 8000, 4 * 4, ..., 154 * 154, 154 * 155.
  Then 2 * 2 * 2, 2 * 2 * 3, ...
  Then 2 * 2 * 2 * 2, ...
  Until 2 * 2 * 2...2 exceeds 24000.

  For each product, for example 2 * 3 * 3 = 18, 2 + 3 + 3 = 8, so it will require 10 ones, making it a 13 element solution.
  Checking each solution against a "best found so far" array,
  the computer will find all of the minimal sum-product numbers for 2 <= k <= 12000.
--}

module Euler88 (e88_solve) where

import Control.Monad.State (put, get, when, mapM_, execState, State)
import qualified Data.Map as Map (Map, findWithDefault, insert, empty, elems, delete)
import qualified Data.Set as Set (foldr, fromList)

e88_solve :: Int
e88_solve = Set.foldr (+) 0 . Set.fromList . Map.elems $ solve

limit :: Int
limit = 12 * 10 ^ 3

solve :: Map.Map Int Int
solve = Map.delete 1 . execState (solve' 1 1 1 2) $ Map.empty

solve' :: Int -> Int -> Int -> Int -> State (Map.Map Int Int) ()
solve' p s n from
  | k > limit = return ()
  | otherwise = do
      check k p >>= (\test -> when test $ insert' k p)
      let
        to = limit * 2 `div` p
      mapM_ (\x -> solve' (p * x) (s + x) (succ n) x) [from..to]
  where
    k = p - s + n

check :: Int -> Int -> State (Map.Map Int Int) Bool
check k p = get >>= (\m -> return (p < Map.findWithDefault (limit * 3) k m))

insert' :: Int -> Int -> State (Map.Map Int Int) ()
insert' k p = get >>= (put . Map.insert k p) >> return ()

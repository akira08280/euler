{--
  https://projecteuler.net/problem=74

  My code is ugly and long.
  
  but it was good training for me to learn about State monad.
  I didn't clealy understand State monad until now.
  It's important for me that we don't need pass the argument in context of State monad.
--}

module Euler74 (e74_solve) where

import Control.Monad.State (put, get, forM_, when, evalState, State)
import Data.Char (digitToInt)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- sum of factorial by each digit
sum' :: Int -> Int
sum' = sum . map ((a !!) . digitToInt) . show
  where
    a = [1,1,2,6,24,120,720,5040,40320,362880]

-- clear temporary set for check duplicate
clearChain :: Int -> State (Int, Int, Map Int Int, Set Int) ()
clearChain a = do
  (criteria, count, cache, chain) <- get
  put (criteria, a, cache, Set.empty)

-- add element to temporary set for check duplicate
rememberChain :: Int -> State (Int, Int, Map Int Int, Set Int) ()
rememberChain a = do
  (criteria, count, cache, chain) <- get
  put (criteria, count, cache, Set.insert a chain)

-- check duplicated or existed on cache
isDuplicate :: Int -> State (Int, Int, Map Int Int, Set Int) Bool
isDuplicate a = do
  (criteria, count, cache, chain) <- get
  case a of
    a
      | a `Set.member` chain -> do clearChain (Set.size chain); return True
      | a `Map.member` cache -> do clearChain (Set.size chain + (cache ! a)); return True
      | otherwise -> do rememberChain a; return False

-- make chain by sum of factorial, until duplicated or existed on cache
makeChain :: [Int] -> State (Int, Int, Map Int Int, Set Int) [Int]
makeChain [] = return []
makeChain (a:as) = do
  test <- (fmap not . isDuplicate) a
  if test
  then do rest <- makeChain as
          return (a:rest)
  else return []

-- update for cache setting chain count
updateCache :: (Int, Int) -> State (Int, Int, Map Int Int, Set Int) ()
updateCache (a, b) = do
  (criteria, count, cache, chain) <- get
  put (criteria, count, Map.insert a b cache, chain)

-- calculate which chain length is over criteria
count' :: Int -> Int -> State (Int, Int, Map Int Int, Set Int) Int
count' n m = do
  forM_ [1..n] $ \i -> do
    c <- makeChain $ iterate sum' i
    (criteria, count, cache, chain) <- get
    mapM_ updateCache $ zip c [count,count-1..]
    when (count == m) $
      put (succ criteria, count, cache, chain)
  get >>= (\(criteria ,_,_,_) -> return criteria)

-- eval state
solve :: Int -> Int -> Int
solve n m = evalState (count' n m) (criteria, count, cache, chain)
  where
    criteria = 0
    count = 0
    cache = Map.fromList [(169,3),(871,2),(872,2),(1454,3),(363601,3),(45361,2),(45362,2)]
    chain = Set.empty

e74_solve :: Int
e74_solve = solve (10 ^ 6) 60

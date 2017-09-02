{--
  https://projecteuler.net/problem=37

  Primes that is able to append left side of single digit primes like [2,3,5,7] are
  [13,23,43,53,73,83,17,37,47,67,97]

  Simiraly, primes that append right side of sibgle digit primes like [2,3,5,7] are
  [23,29,31,37,53,59,71,73,79]

  Common set of these two is primes that is able to truncate digit from both side.
  [23,37,53,73]

  We can't append [1,3,7,9] of right side, but left side is able to append all digits like [1..9].
  So, we continue until either of two set is empty.
--}

module Euler37 (e37_solve) where

import Data.Numbers.Primes (isPrime)
import qualified Data.Set as Set (Set, elems, fromList, intersection, union, empty)

e37_solve :: Int
e37_solve = sum . make $ [2,3,5,7]

make :: [Int] -> [Int]
make ps = Set.elems $ make' ps ps Set.empty
  where
    make' :: [Int] -> [Int] -> Set.Set Int -> Set.Set Int
    make' [] _ a = a
    make' _ [] a = a
    make' r l a  = make' nr nl na
      where
        nr = filter isPrime $ right r
        nl = filter isPrime $ left l
        na = Set.union a $ Set.intersection (Set.fromList nr) (Set.fromList nl)

right :: [Int] -> [Int]
right []     = []
right (p:ps) = map (\e -> p * 10 + e) [1,3,7,9] ++ right ps

left :: [Int] -> [Int]
left []     = []
left (p:ps) = map (\e -> e * 10 ^ (digit p) + p) [1..9] ++ left ps

digit :: Int -> Int
digit = length . show

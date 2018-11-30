{--
  https://projecteuler.net/problem=23
    
  The range to check unable to sum two abundant numbers is 0 <= n/2.
  Pair numbers is symmetry origin n/2.

  [12,18,20,24,30]
  
  30 = 12 + 18 (15 +- 3)
  40 = 20 + 20 (20 +- 0)
  38 = 18 + 20 (19 +- 1)
--}

module Euler23 (e23Solve) where

import NumberTheory (divisors)
import qualified Data.Set as Set (fromList, notMember)

e23Solve :: Integral a=> a
e23Solve =
  let
    upperBound  = 28123
    abList      = filter (\e -> e < sumDivisors e) [1..upperBound]
    abSet       = Set.fromList abList
    notSumAbs n = all (\e -> Set.notMember (n - e) abSet) $ takeWhile (<= div n 2) abList
  in
    sum . filter notSumAbs $ [1..upperBound]

sumDivisors :: Integral a => a -> a
sumDivisors n = sum . takeWhile (< n) $ divisors n

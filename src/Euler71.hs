{--
  https://projecteuler.net/problem=71

  Faray sequence
  [https://en.wikipedia.org/wiki/Farey_sequence]

  If p%q has neighbours a%b and c%d in some Faray sequence, with
  a % b < p % q < c % d
  then p%q is the medient of a%b and c%d -- in other words,
  p % q = (a + c) % (b + d)

  3 % 8,2 % 5,3 % 7,1 % 2
  
  We want to find the fraction immediately to the left of 3 % 7.
  At the beginning, we calc 5 % 12 which is the medient of 2 % 5 and 3 % 7. 5 % 12 is surely the left of 3 % 7.
  Next, we calc 8 % 19 which is the medient of 5 % 12 and 3 % 7. 8 % 19 is surely the left of 3 % 7, too.
  we repeat above calclation until denominator exceeding 10 ^ 6.
--}

module Euler71 (e71Solve) where

import Control.Arrow ((***))

e71Solve :: Integral a => a
e71Solve = fst . last . takeWhile ((< 10 ^ 6) . snd) . iterate ((+ 3) *** (+ 7)) $ (2,5)

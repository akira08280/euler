{--
  https://projecteuler.net/problem=106

  When n is a natural number, a natural number from 1 to 2 n,

  a1 < a2 < a3 < .. < an
  ^    ^    ^         ^
  b1 < b2 < b3 < .. < bn

  The number satisfying the above is Catalan number n.
  https://qiita.com/y_minoda/items/ea283aef6323a541db34#%E3%82%BD%E3%83%BC%E3%82%B9
--}

module Euler106 (e106_solve) where

import SpecialSubset (countTestCase)

e106_solve :: Int
e106_solve = countTestCase 12 

{--
  https://projecteuler.net/problem=142

  a = x + y
  b = x - y
  c = x + z
  d = x - z
  e = y + z
  f = y - z

  f = y - z = (x + y) - (x + z) = a - c
  e = y + z = (x + y) - (x - z) = a - d
  b = x - y = (x + z) - (y + z) = c - e

  Therefore we can know the values which are a and c and d; we can reach the values which are f and e and b.
  Moreover, it follows the below formulas because of the above equations.

  x = (a + b) / 2
  y = (e + f) / 2
  z = (c - d) / 2

  We can conduct that a and b and e and f are the same parity due to the above equations.
  Furthermore, because of e = a - d and f = a - c, we can also conduct that c and "d" are same sign.
  In addition to,  x > y > z > 0, it follows that a > c > d.
--}

module Euler142 (e142_solve) where

import Control.Monad (guard)
import Common (isSquare)

e142_solve :: Integral a => a
e142_solve = sum . head $ findXyz

findXyz :: Integral a => [[a]]
findXyz = do
  a' <- [4..]
  c' <- [3..a']
  let
    a = a' ^ 2
    c = c' ^ 2
    f = a - c
  guard (f > 0)
  guard (isSquare f)
  let
    start = startFromParity c
  d' <- [start,start+2..c']
  let
    d = d' ^ 2
    e = a - d
    b = c - e
  guard (e > 0)
  guard (b > 0)
  guard (isSquare e)
  guard (isSquare b)
  let
    x = (a + b) `div` 2
    y = (e + f) `div` 2
    z = (c - d) `div` 2
  return [x, y, z]

startFromParity :: Integral a => a -> a
startFromParity n
  | odd n = 1
  | even n = 2

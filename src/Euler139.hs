{--
  https://projecteuler.net/problem=139

  Let the triplets are of the form (a, b, c)

  (Central hole) + (four triangles) = (the area of square)
  (b - a)^2      + 2ab              = c^2

  c^2 must be evenly divisible by (b - a)^2.
  From the above formula; it follows that (b - a)^2 must evenly divide 2ab.

  In a primitive Pythagorean triple, a and b have no common factors greater than 1.
  Furthermore, whether a and b is odd, and the other even.
  Because of let "m" and "n" positive integer,

  a^2 + b^2 = c^2 is m^2 -n^2 + 2mn = m^2 + n^2.
  (2mn is even, so m^2-n^2 is odd)

  Therefore (b - a)^2 is odd since a and b are coprime,
  (b-a) isn't a factor of either a or b.
  2ab is evenly divisible (b - a)^2 if and only if b - a = +-1.

  a^2 + (a + 1)^2 = c^2
  2a^2 + 2a + 1 -c^2 = 0
  4a^2 + 4a + 1 -2c^2 + 1 = 0
  (2a + 1)^2 - 2c^2 = -1

  let 2a + 1 to A,

  A^2 - 2c^2 = -1

  https://www.alpertron.com.ar/QUAD.HTM

  Moreover, we have to count up multiplying until a + b + c < 10^8.

  a + b + c = a + a + 1 + c
            = 2a + 1 + c
            = A + c
--}

module Euler139 (e139Solve) where

e139Solve :: Integral a => a
e139Solve = sum . map countSimilarTriangle . takeWhile isPerimUnderLimit . tail . iterate nextXY $ (1, 1)

nextXY :: Integral a => (a, a) -> (a, a)
nextXY (x, y) = (3 * x + 4 * y, 2 * x + 3 * y)

countSimilarTriangle :: Integral a => (a, a) -> a
countSimilarTriangle (x, y) = limit `div` (x + y)

isPerimUnderLimit :: Integral a => (a, a) -> Bool
isPerimUnderLimit (x, y) = x + y < limit

limit :: Integral a => a
limit = 10 ^ 8

{--
  https://projecteuler.net/problem=85

  At first, we had better think about from the minimum
  to figure out how many are there rectangles in the grid.

  In case of the grid of 2 * 2,
   _ _
  |_|_|
  |_|_|

  It's the combination that take 2 from 3 of _ (Y axis), and take 2 from 3 of | (X axis).
  therefore it is derived the generalized following formula.

  |x + 1|   |y + 1|
  |     | * |     |
  |  2  |   |  2  |

  |n|       n!
  | | means --------
  |k|       k!(n-k)!

  Therefore, the number of rectangles in the x * y grid is

  |x + 1|   |y + 1|   (x+1)!       (y+1)!      x(x+1)*y(y+1)
  |     | * |     | = ---------- * --------- = -------------
  |  2  |   |  2  |   2!(x+1-2)!   2!(y+1-2)   4
--}

module Euler85 (e85_solve) where

import Data.List (minimumBy)
import Data.Ord (comparing)

e85_solve :: Integral a => a
e85_solve = fst . minimumBy (comparing snd) $ rectangles

rectangles :: Integral a => [(a, a)]
rectangles = do
  x <- [2..100]
  y <- [x..100]
  let
   limit = 2 * 10 ^ 6
   diff  = abs . (subtract limit) . (flip div 4) $ (x * (succ x)) * (y * (succ y))
   area  = x * y
  return (area, diff)

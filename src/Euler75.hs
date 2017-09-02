{--
  https://projecteuler.net/problem=75

  let L be sum of the edges of right triangles.

  m ^ 2 + n ^ 2 + 2mn + m ^ 2 - n ^ 2 <= L
  2m ^ 2 + 2m                         <= L
  m ^ 2 + m                           <= L % 2
  m ^ 2 < m ^ 2 + m                   <= L % 2
  
  That means the upper limit of L is

  m ^ 2 < L % 2
  m     < sqrt (L % 2)
--}

module Euler75 (e75_solve) where

import Control.Monad (guard)
import Data.List (group, sort)

e75_solve :: Int
e75_solve = length . concat . filter ((== 1) . length) . group . triangles $ 15 * 10 ^ 5

similar :: Integral a => a -> a -> [a]
similar limit s = takeWhile (<= limit) . map (* s) $ [1..]

triangles :: Integral a => a -> [a]
triangles limit = sort . concatMap (similar limit) . triangles' $ limit

triangles' :: Integral a => a -> [a]
triangles' limit = do
  let
    upperM = floor . sqrt . fromIntegral . div limit $ 2
  m <- [2..upperM]
  n <- [1..m-1]
  guard (odd (m + n))
  guard (gcd m n == 1)
  let
    a = m ^ 2 - n ^ 2
    b = 2 * m * n
    c = m ^ 2 + n ^ 2
  guard (a + b + c <= limit)
  return (a + b + c)

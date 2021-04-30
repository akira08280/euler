{--
  https://projecteuler.net/problem=173

  n,m -> *********
  n   -> *       *
  n   -> *       *
  n   -> *       *
  n   -> *       *
  n   -> *       *
  n   -> *       *
  n   -> *       *
  n   -> *********

  n,m ->  ******
  n,m ->  ******
  n   ->  **  **
  n   ->  **  **
  n   ->  ******
  n   ->  ******

  Let n be outer tiles count and let m be the outer layer count.
  Therefore the formula to count all tiles is

  f = n^2 - (n - 2m)^2 = 4m(n - m)
  f/4m = n - m

  Moreover, subtracting m from every tile count can count up every case of the same layer.

  f/4m - m = n - 2m

  ********* ******** ******* ****** ***** **** ***
  *       * *      * *     * *    * *   * *  * * *
  *       * *      * *     * *    * *   * *  * ***
  *       * *      * *     * *    * *   * ****
  *       * *      * *     * *    * *****
  *       * *      * *     * ******
  *       * *      * *******
  *       * ********
  *********

  Finally, it is unnecessary to count up until 1000000 because one edge is not more than 1000.

--}

module Euler173 (e173Solve) where

e173Solve :: Integer
e173Solve = sum . map (\x -> m `div` x - x) $ [1..upper]
  where
    upper = ceiling . sqrt . fromIntegral $ m

m :: Integral a => a
m = tiles `div` 4 

tiles :: Integral a => a
tiles = 10 ^ 6

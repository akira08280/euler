{--
  https://projecteuler.net/problem=92

  I couldn't think of using dynamic programming.
  This site gave me new perspective. I really appreciate it.

  [http://blog.dreamshire.com/project-euler-92-solution/]

  I try to explain the part of dp.

  1288000 -> 1 + 4 + 64 + 64 +  0 +  0 + 0 = 133
  1234590 -> 1 + 4 + 9  + 16 + 25 + 81 + 0 = 133

  So, 133 of 7 digit means 128800, 8281800, 1234590, 1324950, 9504312 and so on.
  And the previous state is 6 digit.

  So there are 123450 (133 - 81) and 128000 (133 - 64) and 123490 (133 - 25) and 123590 (133 - 16) and
               124590 (133 - 9)  and 134590 (133 - 4)  and 234590 (133 - 1)  and 123459 (133 - 0).

  Of cource, the above case is an example. there are many case of each subtract number.
  This is the meaning of 'dynamic' function.
--}

module Euler92 (e92_solve) where

import Data.Char (digitToInt)

e92_solve :: Integral a => a
e92_solve =
  let
    before = map (\a -> if a `elem` squares then 1 else 0) [0..limit]
    after  = foldl dynamic before [2..digits] -- from second digit to seventh digit.
  in
    sum . map (after !!) . filter unhappy $ [1..limit]

dynamic :: Integral a => [a] -> a -> [a]
dynamic state _ = map f [0..limit]
  where
    f n = sum . map ((state !!) . flip subtract n) . takeWhile (<= n) $ squares

unhappy :: Int -> Bool
unhappy = (> 1) . head . dropWhile (\e -> e > 1 && e /= 1 && e /= 89) . iterate sumPow
  where
    sumPow = sum . map ((^ 2) . digitToInt) . show

digits :: Integral a => a
digits = 7

limit :: Integral a => a
limit = 9 ^ 2 * digits

squares :: Integral a => [a]
squares = [0,1,4,9,16,25,36,49,64,81]

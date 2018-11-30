{--
  https://projecteuler.net/problem=68

  Analysis:
  http://www.mathblog.dk/project-euler-68-what-is-the-maximum-16-digit-string-for-a-magic-5-gon-ring/

  1. Since string cosists of 17-digit with count 10 twice, 10 has to be in the outer ring.
  2. We have to find the maximum number starting with minimum number in the outer ring,
     so we assume that outer is {6,7,8,9,10}, inner is {1,2,3,4,5}
  3. There are five strings to concat, and sum of digit is 2*(1+2+3+4+5)+6+7+8+9+10=70.
     Therefore sum of digit of each string is 14.
  4. From 2, we have to start with 6. and since sum of digit is 14, so we put first 6-5-3.
     (It's not 6-3-5, because we have to find maximum number.)
  5. Since string including 10 cannot consist of numbers except 10-3-1, so next string is 10-3-1.
  6. After that we eliminate the impossible.
--}

module Euler68 (e68Solve) where

e68Solve :: Integral a => a
e68Solve = 6531031914842725

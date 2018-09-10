{--
  https://projecteuler.net/problem=104
--}

module Euler104 (e104_solve) where

import Common (isPandigital)

e104_solve :: Int
e104_solve = fst . head . filter panCheck . zip [0..] $ fibsOnTailCut

panCheck :: Integral a => (a, Int) -> Bool
panCheck (i, e) = isPanTail && isPanHead
  where
    isPanTail = isPandigital e [1..9]
    isPanHead = isPandigital (floor (10 ** (decimalPart + 8))) [1..9]
      where
        t = fromIntegral i * 0.20898764024997873 - 0.3494850021680094
        integerPart = fromIntegral . floor $ t
        decimalPart = t - integerPart

fibsOnTailCut :: [Int]
fibsOnTailCut = map fst $ iterate (\(a, b) -> (b, (a + b) `mod` 1000000000)) (0, 1)

{--
  https://projecteuler.net/problem=36
--}

module Euler36 (e36_solve) where

e36_solve :: Integral a => a
e36_solve =
  let
    upper = 10 ^ 6
    p1 = takeWhile (< upper) $ map mp1 [1..]
    p2 = takeWhile (< upper) $ map mp2 [1..]
    ps = p1 ++ p2
  in
    sum . filter (`isp` 2) $ ps

mp1 :: Integral a => a -> a
mp1 n = mp' n n

mp2 :: Integral a => a -> a
mp2 n = mp' (div n 10) n

mp' :: Integral a => a -> a -> a
mp' n p
  | n <= 0 = p
  | otherwise = mp' (fst dm) (p * 10 + snd dm)
  where
    dm = divMod n 10

isp :: Integral a => a -> a -> Bool
isp n b = isp' n 0
  where
    isp' k r
      | k <= 0 = r == n
      | otherwise = isp' (fst dm) (b * r + snd dm)
      where
        dm = divMod k b

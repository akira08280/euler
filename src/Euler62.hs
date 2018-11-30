{--
  https://projecteuler.net/problem=62
--}

module Euler62 (e62Solve) where

import Data.Function (on)
import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)

e62Solve :: Integer
e62Solve =
  let
    a = map (\e -> (,) e . f . (^ 3) $ e) [345..10 ^ 4]
  in
    (^ 3) . fst . minimum . head . filter ((== 5) . length) . groupBy ((==) `on` snd) . sortBy (comparing snd) $ a

f :: Integer -> Integer
f = read . sortBy (flip compare) . show

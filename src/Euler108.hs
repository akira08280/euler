{--
  https://projecteuler.net/problem=108
--}

module Euler108 (e108Solve) where

import Data.Numbers.Primes (primes)
import Control.Arrow ((&&&))

e108Solve :: Integer
e108Solve = minimum . map snd . filter ((> 2000) . fst) . map (countPrimeFactorsByExp &&& calcNumByExp) $ exps 2 7

countPrimeFactorsByExp :: Integral a => [a] -> a
countPrimeFactorsByExp = product . map (succ . (*) 2)

calcNumByExp :: Integral a => [a] -> a
calcNumByExp = product . zipWith (^) primes

exps :: (Num t, Enum t) => t -> Int -> [[t]]
exps _ 0 = [[]]
exps m 1 = map (:[]) [1..m]
exps m s = yss ++ xss
  where
    yss = exps m (pred s)
    xss = [ xs | ys <- yss,
                 length ys == s - 1,
                 let lst = last ys,
                 xs <- map ((ys ++) . (:[])) [lst,lst-1..1] ]

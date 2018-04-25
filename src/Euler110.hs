{--
  https://projecteuler.net/problem=110
--}

module Euler110 (e110_solve) where

import Data.Numbers.Primes (primes)
import Control.Arrow ((&&&))

e110_solve :: Integer
e110_solve = minimum . map snd . filter ((> 8000000) . fst) . map (countPrimeFactorsByExp &&& calcNumByExp) $ exps 4 20

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

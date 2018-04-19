{--
  https://projecteuler.net/problem=108
--}

module Euler108 (e108_solve) where

import Data.Numbers.Primes (primes)
import Data.List (nub)
import Control.Arrow ((&&&))

e108_solve :: Integer
e108_solve = minimum . map snd . filter ((> 2000) . fst) . map (countPrimeFactorsByExp &&& calcNumByExp) $ exps 2 7

countPrimeFactorsByExp :: Integral a => [a] -> a
countPrimeFactorsByExp = product . map (succ . (*) 2)

calcNumByExp :: Integral a => [a] -> a
calcNumByExp = product . zipWith (^) primes

exps :: (Num a, Num t, Eq a, Eq t, Enum t, Enum a) => t -> a -> [[t]]
exps _ 0 = [[]]
exps m 1 = map (:[]) [1..m]
exps m s = nub $ yss ++ xss
  where
    yss = exps m (pred s)
    xss = [ xs | ys <- yss,
                 let lst = last ys,
                 xs <- map ((ys ++) . (:[])) [lst,lst-1..1] ]

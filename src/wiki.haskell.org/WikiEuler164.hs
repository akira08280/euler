{--
  https://wiki.haskell.org/Euler_problems/161_to_170#Problem_164
--}

module WikiEuler164 (wikiE164Solve) where

addDigit x = [[sum [x !! b !! c | c <- [0..9-a-b]] | b <- [0..9-a]] | a <- [0..9]]
x3 = [[10 - a - b | b <- [0..9 - a]] | a <- [0..9]]
x20 = iterate addDigit x3 !! 17
wikiE164Solve = sum [x20 !! a !! b | a <- [1..9], b <- [0..9-a]]

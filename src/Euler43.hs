{--
  https://projecteuler.net/problem=43

  refer to :
  http://www.mathblog.dk/project-euler-43-pandigital-numbers-sub-string-divisibility/
  
  Let the pandigital number is [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10].

  1. {d4,d5,d6} is divisible 5. d6 = {0,5}.
  2. {d6,d7,d8} is divisible 11, but if d6 is 0, {d6,d7,d8} duplicates digits like {011,022,033..}. Since {d6} = 5.
  3. {d6} = 5, so {d6,d7,d8} = {506, 517, 528, 539, 561, 572, 583, 594}
  4. {d7,d8,d9} is divisible 13. Merging 3, {d6,d7,d8,d9} = {5286,5390,5728,5832}
  5. {d8,d9,d10} is divisible 17. Merging 4, {d6,d7,d8,d9,d10} = {52867, 53901, 57289}
  6. {d5,d6,d7} is divisible by 7. The result of 5, last 2 digits is {52, 53, 57} becaulse of 3 digits of divisible by 7. {d5,d6,d7,d8,d9,d10} = {952867, 357289}
  7. {d2,d3,d4} is divisible by 2. So d4 is even. {d4,d5,d6,d7,d8,d9,d10} = {0952867, 4952867, 0357289, 4357289, 6357289}
  8. {d3,d4,d5} is divisible by 3. So d3+d4+d5 % 3 == 0. Merging 7, {d3,d4,d5,d6,d7,d8,d9,d10} = {30952867, 60357289, 06357289}
  9. The result of 8, d1,d2 = {1,4}
--}

module Euler43 (e43Solve) where

e43Solve :: Int
e43Solve = sum . map (\p -> (read . concat $ p) :: Int) $ pandigitals

pandigitals :: [[String]]
pandigitals = do
  a <- ["1","4"]
  b <- ["30952867", "60357289", "06357289"]
  return [a, b]

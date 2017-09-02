{--
  https://wiki.haskell.org/Euler_problems/61_to_70#Problem_67	

  for example.

     3
    7 4
   2 4 6
  8 5 9 3

  foldr1 step [[3],[7,4],[2,4,6],[8,5,9,3]] means [3] `step` ([7,4] `step` ([2,4,6] `step` [8,5,9,3]).

  step [2,4,6] [8,5,9,3] = 2 + max 8 5 : step [4,6] [5,9,3]
  step [4,6] [5,9,3] = 4 + max 5 9 : step [6] [9,3]
  step [6] [9,3] = 6 + max 9 3 : step [] [3]
  step [] [3] = []

  6+9 : []
  4+9 : [15]
  2+8 : [13,15]
  [10,13,15]

  step [7,4] [10,13,15] = 7 + max 10 13 : step [4] [13,15]
  step [4] [13,15] = 4 + max 13 15 : step [] [15]
  step [] [15] = []

  4+15 : []
  7+13 : [19]
  [20,19]

  step [3] [20,19] = 3 + max 20 19 : step [] [19]
  step [] [19] = []

  [23]
--}

module WikiEuler67 (wiki_e67_solve) where

wiki_e67_solve = readFile "src/resources/p067_triangle.txt" >>= print . solve . parse
parse = map (map read . words) . lines
solve = head . foldr1 step
step [] [_] = []
step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

{--:
  https://wiki.haskell.org/Euler_problems/61_to_70#Problem_61

  permute [1,2,3] means as follow,
  
  1 : permute [2,3]
  |   |
  |   2 : permute [3]
  |   |   |
  |   |   3 : permute [] [1,2,3]
  |   |
  |   3 : permute [2]
  |       |
  |        2 : permute [] [1,3,2]
  |        
  2 : permute [1,3]
  |   |
  |   1 : permute [3]
  |   |   |
  |   |   3 : permute [] [2,1,3]
  |   |
  |   3 : permute [1]
  |       |
  |       1 : permute [] [2,3,1]
  |
  3 : permute [1,2]
      |
      1 : permute [2]
      |   |
      |   2 : permute [] [3,1,2]
      |
      2 : permute [1]
          |
          1 : permute [] [3,2,1]
 
  main :: IO ()
  main = print $ hoge [1..10]

  hoge :: [Int] -> [Int]
  hoge [] = []
  hoge (x:y:z:a) = a
--}       

module WikiEuler61 (wiki_e61_solve) where

import Data.List

permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs

figurates n xs = filter link $ concatMap (gather (map poly xs) . (: [])) (poly n)
  where gather [xs] (v:vs)
          = let v' = match xs v
            in if null v' then [] else map (:v:vs) v'
        gather (xs:xss) (v:vs)
          = let v' = match xs v
            in if null v' then [] else concatMap (gather xss . (: v : vs)) v'
        match xs (_,v) = let p = (v `mod` 100)*100 in sublist (p+10,p+100) xs
        sublist (s,e) = takeWhile (\(_,x) -> x<e) . dropWhile (\(_,x) -> x<s)
        link ((_,x):xs) = x `mod` 100 == snd (last xs) `div` 100
        poly m = [(n, x) | (n, x) <- zip [1..] $ takeWhile (< 10000) $ scanl (+) 1 [m - 1,2 * m - 3..], 1010 < x, x `mod` 100 > 9]

wiki_e61_solve = sum $ map snd $ head $ concatMap (figurates 3) $ permute [4..8]

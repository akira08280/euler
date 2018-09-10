{--
  https://wiki.haskell.org/Euler_problems/81_to_90#Problem_84

  In the game, Monopoly, find the three most popular squares when using two 4-sided dice.

  This may not be the shortest or the fastest implementation, but I hope it is one of the clearest.
  I have one comment about the experience of solving this problem that I would like to share with you.
  At first I thought I would have to make use of the Control.Monad.State library, but being relatively new to Haskell,
  I quickly found myself in the slough of type checker despond.
  It was then that I remembered that foldl/foldr can used instead of "State,"
  and now I found myself in the celestial city of type checker heaven,
  with Haskell preventing me from making silly mistakes at every turn.
  HenryLaxen May 7, 2008
--}

module WikiEuler84 (wiki_e84_solve) where

import Data.Array.IArray
import Data.List
import Data.Ord
import System.Random

data Squares = 
  GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL |
  C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP |
  E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J |
  G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
  deriving (Eq,Ord,Enum,Read,Show,Ix)

type Roll = [Int]

data Cards = GoTo Squares | R | U | Back3 | Other
  deriving (Eq,Ord,Read,Show)

type Deck = [Cards]

data GameState = GameState
  { position :: Squares,
    doublesCount :: Int,
    chance :: [Cards],
    communityChest :: [Cards],
    history :: [Squares]
  } deriving (Eq,Ord,Read,Show)

deckCommunityChest = [ GoTo JAIL, GoTo GO ] ++ replicate 14 Other
deckChance = [ GoTo GO, GoTo JAIL, GoTo C1,
               GoTo E3, GoTo H2, GoTo R1] ++
               [ R, U, Back3] ++
               replicate 6 Other

doubles :: Roll -> Bool
doubles r = head r == r !! 1

defaultGameState = GameState
  { position = GO,
    doublesCount = 0,
    chance = deckChance,
    communityChest = deckCommunityChest,
    history = [GO]
  }

takeCard :: Deck -> (Cards,Deck)
takeCard (c:cs) = (card,deck)
  where card = c
        deck = cs ++ [card]

nextR g = case position g of
  CH1 -> R2
  CH2 -> R3
  CH3 -> R1

nextU g = case position g of
  CH1 -> U1
  CH2 -> U2
  CH3 -> U1

doCommunityChest :: GameState -> GameState
doCommunityChest g =
  let (card,deck) = takeCard (communityChest g)
      rotate g = g {communityChest = deck }
      cases = case card of
        GoTo sq -> g { position = sq }
        Other -> g
  in rotate cases

doChance :: GameState -> GameState
doChance g =
  let (card,deck) = takeCard (chance g)
      rotate g = g {chance = deck }
      cases = case card of
        GoTo sq -> g { position = sq}
        R -> g { position = nextR g }
        U -> g { position = nextU g }
        -- you might back up from CH3 to CC3 so checkForCards again
        Back3 -> checkForCards (g { position = position (newPosition g (-3))})
        Other -> g 
  in rotate cases

newPosition :: GameState -> Int -> GameState
newPosition g n = g {position = toEnum $
  (fromEnum (position g) + n) `mod` (fromEnum H2 + 1)}

checkForCards :: GameState -> GameState
checkForCards g
  | position g `elem` [CH1, CH2, CH3] = doChance g
  | position g `elem` [CC1, CC2, CC3] = doCommunityChest g
  | otherwise = g

travel :: GameState -> [Int] -> GameState
travel g roll =
  let value = sum roll
      checkDoubles
        | doubles roll && doublesCount g == 2 =
          g { position = JAIL,
              doublesCount = 0 }
        | doubles roll = move $ g { doublesCount = doublesCount g + 1}
        | otherwise = move $ g { doublesCount = 0}
      move g = newPosition g value
      checkForJail g
        | position g == G2J = g { position = JAIL }
        | otherwise = g
      saveHistory g = g { history = position g : history g }
  in saveHistory $ checkForCards $ checkForJail checkDoubles

game :: GameState -> [Roll] -> GameState
-- As an exercise in what a difference strictness can make
-- compare the performance of this with replacing foldl' by foldl
game = foldl' travel

statistics :: [Squares] -> [(Squares, Float)]
statistics history =
  let a = accumArray (+) 0 (GO,H2) (zip history (repeat 1)) :: Array Squares Int
      b = assocs a
      c = sortBy (flip (comparing snd)) b
      (sq,cnt) = unzip c  -- wiki formatting bug, should be unzip c
      total = sum cnt
      stats = map (\x -> fromIntegral x / fromIntegral total * 100) cnt
  in take 3 $ zip sq stats

r = [[1,1],[2,2],[2,2],[4,4]]
t = game defaultGameState r  -- useful for debugging

pairs :: [a] -> [[a]]
pairs [] = [[]]
pairs (x:y:xs) = [x,y] : pairs xs

dieSides :: (Int,Int)
-- dieSides = (1,6)
dieSides = (1,4)
maxRolls = 1000000

--wiki_e84_solve :: IO ()
wiki_e84_solve = do
  seed <- newStdGen
  let rolls = pairs (randomRs dieSides seed)
      stats = statistics (history (game defaultGameState
        (take maxRolls rolls)))
      result = map (fromEnum . fst) stats
  return (stats,result)

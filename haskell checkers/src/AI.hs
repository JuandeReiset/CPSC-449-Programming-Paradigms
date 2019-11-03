module AI where

import GameStructures
import Moves

data Tree = Leaf | Node Int Tree Tree 
                  deriving Show

tGameState :: GameState
tGameState =
  GameState { _blackPieces = bInit
            , _redPieces = rInit
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = ""}


                       
bInit :: [Coord]
bInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

rInit :: [Coord]
rInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]


red_ai::GameState -> Move 
red_ai s = []
{-
red_ailist::GameState -> [int]
red_ailist s = map red_heuristic (moves s)
-}
black_ai::GameState -> Move
black_ai s = []
{-
-- | Principal Variantions
data PV = PV !Int [Move] deriving (Show)

instance Eq PV where
    (PV x _) == (PV y _) = x==y

instance Ord PV where
    compare (PV x _) (PV y _) = compare x y

negatePV :: PV -> PV
negatePV (PV x ms) = PV (-x) ms

-- | Minimax with alpha-beta pruning
-- | extended with score and principal variation 
minimaxPV :: Tree Int Move -> (Int, [Move])
minimaxPV bt 
    = case minimaxPV_ab' 0 [] (PV (-infinity-1) []) (PV (infinity+1) []) bt of
        PV v ms -> (v,ms)

-- | first parameter determines if we negate children scores
-- | minimaxPV_ab' :: (Num a, Ord a) => Int -> [m] -> a -> a  -> GameTree a m -> (a, [m])
minimaxPV_ab' depth ms a b (Tree x []) = a `max` PV x (reverse ms) `min` b
minimaxPV_ab' depth ms a b (Tree _ branches) = cmx a b branches
    where cmx a b [] = a
          cmx a b ((m,t) : branches) 
              | a'==b = a'
              | otherwise = cmx a' b branches
              where a'| odd depth = negatePV $ minimaxPV_ab' (1+depth) (m:ms) (negatePV b) (negatePV a) t
                      | otherwise = minimaxPV_ab' (1+depth) (m:ms) a b t

-}
red_heuristic::GameState -> Int
red_heuristic s
 | (_blackPieces s) == [] && (_blackKings s) == []
  = 10000
 | (_redPieces s) == [] && (_redKings s) == []
  = (-10000)
 |otherwise = (count_elem (_redPieces s)  0) - 
               (count_elem (_blackPieces s)  0) + 
               2*((count_elem (_redKings s)  0) -
               (count_elem (_blackKings s) 0))

black_heuristic::GameState -> Int
black_heuristic s 
 | (_redPieces s) == [] && (_redKings s) == []
  =  10000
 | (_blackPieces s) == [] && (_blackKings s) == []                
 = (-10000)
 | otherwise =  (count_elem (_blackPieces s) 0) - 
                (count_elem (_redPieces s)  0) +  
                2*((count_elem (_blackKings s)  0) -
                (count_elem (_redKings s)  0))

count_elem::[Coord]->Int-> Int
count_elem (x:xs)  count = count_elem xs  count+1
count_elem [] count = count




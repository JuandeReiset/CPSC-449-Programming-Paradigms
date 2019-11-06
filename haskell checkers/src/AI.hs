module AI where

--import GameStructures
import GameLogic
import Moves
import ApplyMove

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
red_ai s = extractMove s (minmax s 5) (moves s)

{-
red_ailist::GameState -> [int]
red_ailist s = map red_heuristic (moves s)
-}
black_ai::GameState -> Move
black_ai s = extractMove s (minmax s 5) (moves s)

extractMove::GameState->Int->[Move]->Move
extractMove s heuristic (mov:[]) = mov  
extractMove s heuristic (mov:xs)
 | (heuristic == (red_heuristic (applyTHEMove mov s)))
  = mov
 | otherwise = extractMove s heuristic xs

minmax::GameState -> Int ->  Int
minmax s depth
 | (depth == 0) || (_status s) == GameOver
  = red_heuristic s
 | (_status s) == Red
  = maxEval s (moves s) depth (-3000)
 | (_status s ) == Black
  = minEval s (moves s) depth 3000
{-
minmaxblack::GameState -> Int ->  Int
minmaxblack s depth
 | (depth == 0) || (_status s) == GameOver
  = black_heuristic s
 | (_status s) == Black
  = maxrEval s (moves s) depth (-3000)
 | (_status s ) == Red
  = minrEval s (moves s) depth 3000
-}
maxEval::GameState ->[Move]-> Int -> Int -> Int
maxEval s [] depth maxv = maxv
maxEval s (mov:xs) depth maxv = max (max maxv eval) (maxEval s xs depth maxv)
                      where eval = minmax (applyTHEMove mov s) (depth-1)



minEval:: GameState -> [Move]->Int-> Int -> Int
minEval s [] depth minv = minv
minEval s (mov:xs)  depth minv = min (min minv eval) (minEval s xs depth minv)
                      where eval = minmax (applyTHEMove mov s) (depth-1)



red_heuristic::GameState -> Int
red_heuristic s
 | (_blackPieces s) == [] && (_blackKings s) == []
  = 3000
 | (_redPieces s) == [] && (_redKings s) == []
  = (-3000)
 |otherwise = (count_elem (_redPieces s)  0) - 
               (count_elem (_blackPieces s)  0) + 
               2*((count_elem (_redKings s)  0) -
               (count_elem (_blackKings s) 0))
{-
black_heuristic::GameState -> Int
black_heuristic s 
 | (_redPieces s) == [] && (_redKings s) == []
  =  3000
 | (_blackPieces s) == [] && (_blackKings s) == []                
 = (-3000)
 | otherwise =  (count_elem (_blackPieces s) 0) - 
                (count_elem (_redPieces s)  0) +  
                2*((count_elem (_blackKings s)  0) -
                (count_elem (_redKings s)  0))
-}
count_elem::[Coord]->Int-> Int
count_elem (x:xs)  count = count_elem xs  count+1
count_elem [] count = count




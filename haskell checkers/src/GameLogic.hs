{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module GameLogic where

import Lens.Micro.Platform


type ApplyMove = Move -> GameState -> GameState

type AiMove = GameState -> Move

data MoveType = Human | AI AiMove

type Coord = (Int, Int)
type Move = [Coord]

data Status = Red | Black | GameOver 
  deriving (Show, Eq)


data GameState =
  GameState { _blackPieces :: [Coord]
            , _redPieces :: [Coord]
            , _blackKings :: [Coord]
            , _redKings :: [Coord]
            , _status :: Status
            , _message :: String}
              deriving (Show, Eq)

makeLenses ''GameState

{-
initialGameState :: GameState
initialGameState =
 GameState { _blackPieces = [(1,5)]
            , _redPieces = [(2,6)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = "Black Turn."}
  -} 
         
initialGameState :: GameState
initialGameState =
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = "Red Turn."} 
             
testState :: GameState
testState =
 GameState { _blackPieces = [(2,4),(4,4), (6,2),(4,2)]
            , _redPieces = [(3,5)]
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = "Red Turn."}
rk4jumpState:: GameState
rk4jumpState =
 GameState { _blackPieces = [(4,3),(2,5)]
            , _redPieces = []
            , _blackKings = []
            , _redKings = [(3,4)]
            , _status = Red
            , _message = "Red Turn."}        
testGame :: GameState
testGame =
 GameState { _blackPieces = [(5,0),(7,0), (2,1),(4,1),(1,2),(5,2),(0,3),(3,6)]
            , _redPieces = [(1,4),(2,5),(6,3),(7,4),(6,5),(7,6),(6,7)]
            , _blackKings = [(0,7)]
            , _redKings = []
            , _status = Red
            , _message = "Red Turn."}           

p2k :: GameState
p2k =
 GameState { _blackPieces = [ (2,1),(4,1)]
            , _redPieces = [(1,2)]
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = "Red Turn."}           

nomoves :: GameState
nomoves =
 GameState { _blackPieces = [ (0,3)]
            , _redPieces = [(1,4),(2,5)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = "Red Turn."}            

blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]

{-
blackInit :: [Coord]
blackInit = [ (1,0)]

redInit :: [Coord]
redInit = [ (0,7)]
-}
setMessage :: GameState -> GameState
setMessage s = case (_status s) of
  Red -> set message
    "Red Turn." s
  Black -> set message
    "Black Turn." s
  _ -> s
 
applyMove :: Move -> GameState -> GameState
applyMove m s = case s^.status of
 Red -> if ((islegal s m)&&(m `elem` (simple_moves s)) &&(is_there (jump_moves s)))
         then set message "There is a jump move available" s
         else if ((islegal s m)&&(m `elem` (simple_moves s)) && (not(is_there (jump_moves s)))) 
         then if ((head m)`elem`(s^.redPieces)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
         else if ((islegal s m)&&(m `elem` (jump_moves s)))
            then if ((head m)`elem`(s^.redPieces) || (head m)`elem`(s^.redKings))
                  then toogle_status $ apply_jump_move m s
                  else toogle_status $ apply_jump_move m s
        else set message "Invalid move" s
 Black -> if ((islegal s m)&&(m `elem` (simple_moves s)) &&(is_there (jump_moves s)))
           then set message "There is a jump move available" s
           else if ((islegal s m)&&(m `elem` (simple_moves s)) && (not(is_there (jump_moves s)))) 
           then if ((head m)`elem`(s^.blackPieces)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
          else if ((islegal s m)&&(m `elem` (jump_moves s)))
           then if ((head m)`elem`(s^.blackPieces) || (head m)`elem`(s^.blackKings))
               then toogle_status $ apply_jump_move m s
               else toogle_status $ apply_jump_move m s
        else set message "Invalid move" s
 _ -> initialGameState

 {-
applyMove _  s = case s^.status of
  Red -> setMessage $ set status Black s
  Black -> setMessage $ set status Red s
  _ -> initialGameState
 -}

--DECIDE AND DETERMINE IF MOVE IS LEGAL
--all possible moves
moves::GameState -> [Move]
moves s = (simple_moves s) ++ (jump_moves s)
--all possible simple moves
simple_moves::GameState -> [Move]
simple_moves s = case s^.status of
 Red -> rsimple_moves s
 Black -> bsimple_moves s
 _ -> []

--all possible simple moves for red
rsimple_moves::GameState-> [Move]
rsimple_moves s = (rpawn_moves s  (s^.redPieces) [])++(rking_moves s (s^.redKings) [])

--all possible simple moves for red pawns
rpawn_moves::GameState->[Coord]->[Move]->[Move]
rpawn_moves s [] m = m
rpawn_moves s ((x,y):rest) m
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x-1,y-1)]: m)
 |(is_up_right_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x+1,y-1)]: m)
 |otherwise = rpawn_moves s rest m
 
--all possible simple moves for red kings
rking_moves::GameState->[Coord]->[Move]->[Move]
rking_moves s [] m = m
rking_moves s ((x,y):rest) m 
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))  && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:m)
 |(is_up_right_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y))  && (is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_left_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x-1,y-1)]:m)
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y+1)]:m)
  |(is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y+1)]:m)
 |otherwise = rking_moves s rest m
 
 
--all possible simple moves for black
bsimple_moves::GameState->[Move]
bsimple_moves s = (bpawn_moves s  (s^.blackPieces) [])++(bking_moves s (s^.blackKings) [])

bpawn_moves::GameState->[Coord]->[Move]->[Move]
bpawn_moves s [] m = m
bpawn_moves s ((x,y):rest) m
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x-1,y+1)]:[(x,y),(x+1,y+1)]:m)
 |(is_down_left_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x-1,y+1)]: m)
 |(is_down_right_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x+1,y+1)]: m)
 |otherwise = bpawn_moves s rest m
 
--all possible blacck king simple moves
bking_moves::GameState->[Coord]->[Move]->[Move]
bking_moves s [] m = m
bking_moves s ((x,y):rest) m 
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))  && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:m)
 |(is_up_right_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y))  && (is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_left_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x-1,y-1)]:m)
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y+1)]:m)
  |(is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y+1)]:m)
 |otherwise = bking_moves s rest m


--DETERMINE jump_moves in a certain gamestate
-- {-
jump_moves::GameState -> [Move]
jump_moves s = case s^.status of
 Red ->  (r_alljumps s (_redPieces s) []) ++ (rk_alljumps s (_redKings s) [])
 Black -> (b_alljumps s (_blackPieces s) []) ++ (bk_alljumps s (_blackKings s) [])
 _ -> []
-- -}
{-
jump_moves::GameState -> [Move]
jump_moves s 
 |s^.status == Red 
 =(jumpKing (s^.redKings) s) ++ (jumpPawn (s^.redPieces))
 |s^.status == Black 
 =(jumpKing (s^.blackKings) s) ++ (jumpPawn (s^.blackPieces))
 |otherwise = [[]]

jumpKing::[Coord]->GameState->[Move]
jumpKing xs s= [(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y) s]

jumpKing'::Coord->[Coord]->Coord->GameState->[Move]
jumpKing' start rem (x,y) st = [ (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                   , not ( (x',y') `elem` rem) && opponent_occupied (x',y') st && (start==(x'',y'') || notoccupied (x'',y'') st) && onboard (x'',y'')
                   , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'') st) ]

jump_over::[Move]->[Move]
jump_over []  = [[]]
jump_over z  = z

jumpPawn::[Coord] -> [Move]
jumpPawn s = [[]]
-}
-- {-
--red_jump_moves::GameState->[Move]
--red_jump_moves s = (r_jumps s (s^.redPieces) []) -- ++ (rking_jumps s (s^.redKings) [])
{-
rsimple_jumps::GameState->[Coord]->[Move]->[Move]
rsimple_jumps s [] m = m
rsimple_jumps s ((x,y):rest) m
 |(is_up_right_jump_possible s (x,y))&&(is_up_left_jump_possible s (x,y))
  =rsimple_jumps s rest ([(x,y),(x-2,y-2)]:[(x,y),(x+2,y-2)]:m)
 |(is_up_right_jump_possible s (x,y))
  =rsimple_jumps s rest ([(x,y),(x+2,y-2)]: m)
 |(is_up_left_jump_possible s (x,y))
  =rsimple_jumps s rest ([(x,y),(x-2,y-2)]: m)
 |otherwise = rsimple_jumps s rest m
-}

r_alljumps::GameState->[Coord]->[Move]->[Move]
r_alljumps s [] rem = rem
r_alljumps s ((x,y):xs) rem 
 | (r_jumps s (x,y) []) == [[(x,y)]]
  = (r_alljumps s xs rem)
 | otherwise
  = (r_alljumps s xs rem ++ (r_jumps s (x,y) []))


r_jumps::GameState->Coord->Move->[Move]
r_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (y-2 == 0)
  = (rking_jumps s (x-2,y-2) (rem++[(x,y)]))++ (rking_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (r_jumps s (x-2,y-2) (rem++[(x,y)]))++ (r_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (y-2 == 0) 
 = rking_jumps s (x+2,y-2) (rem++[(x,y)])
 | (is_up_right_jump_possible s (x,y)) 
 = r_jumps s (x+2,y-2) (rem++[(x,y)]) 
 | (is_up_left_jump_possible s (x,y)) && (y-2 == 0)
 = rking_jumps s (x-2,y-2) (rem++[(x,y)]) 
 | (is_up_left_jump_possible s (x,y))
 = r_jumps s (x-2,y-2) (rem++[(x,y)])
 |otherwise = [rem ++ [(x,y)]]

rk_alljumps::GameState->[Coord]->[Move]->[Move]
rk_alljumps s [] rem = rem
rk_alljumps s ((x,y):xs) rem 
 | (rking_jumps s (x,y) []) == [[(x,y)]]
  = (rk_alljumps s xs rem)
 | otherwise
  = (rk_alljumps s xs rem ++ (rking_jumps s (x,y) []))

rking_jumps::GameState->Coord->Move->[Move]
rking_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) 
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) 
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y))  
  = (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) 
 | (is_down_left_jump_possible s (x,y))  
  = (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)])) 
 |otherwise = [rem ++ [(x,y)]]



b_alljumps::GameState->[Coord]->[Move]->[Move]
b_alljumps s [] rem = rem
b_alljumps s ((x,y):xs) rem
 | (b_jumps s (x,y) []) == [[(x,y)]]
  = (b_alljumps s xs rem)
 | otherwise
  = (b_alljumps s xs rem ++ (b_jumps s (x,y) []))


b_jumps::GameState->Coord->Move->[Move]
b_jumps s (x,y) rem
 | (is_down_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (y+2 == 7)
  = (bking_jumps s (x-2,y+2) (rem++[(x,y)]))++ (bking_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_down_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (b_jumps s (x-2,y+2) (rem++[(x,y)]))++ (b_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (y+2 ==7) 
 = bking_jumps s (x+2,y+2) (rem++[(x,y)]) 
 | (is_down_right_jump_possible s (x,y)) 
 = b_jumps s (x+2,y+2) (rem++[(x,y)]) 
 | (is_down_left_jump_possible s (x,y)) && (y+2 == 7)
 = bking_jumps s (x-2,y+2) (rem++[(x,y)]) 
 | (is_down_left_jump_possible s (x,y))
 = b_jumps s (x-2,y+2) (rem++[(x,y)]) 
 |otherwise = [rem ++ [(x,y)]]

bk_alljumps::GameState->[Coord]->[Move]->[Move]
bk_alljumps s [] rem = rem
bk_alljumps s ((x,y):xs) rem 
 | (bking_jumps s (x,y) []) == [[(x,y)]]
  = (bk_alljumps s xs rem)
 | otherwise
  = (bk_alljumps s xs rem ++ (bking_jumps s (x,y) []))

bking_jumps::GameState->Coord->Move->[Move]
bking_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) 
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) 
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y))  
  = (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) 
 | (is_down_left_jump_possible s (x,y))  
  = (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)])) 
 |otherwise = [rem ++ [(x,y)]]


islegal::GameState->Move->Bool
islegal s [] = False
islegal s m
 | m `elem` (moves s)  = True 
 | otherwise = False

--HELPER FUNCTIONS--

--Diagonal simple movement helpers---------------
is_up_right_empty::GameState -> Coord -> Bool
is_up_right_empty s (x,y)
 |not((x+1, y-1) `elem` s^.redPieces) &&
  not((x+1, y-1) `elem` s^.redKings) &&
  not((x+1, y-1) `elem` s^.blackPieces) &&
  not((x+1, y-1) `elem` s^.blackKings) &&
  (x+1 <= 7)
  = True
 |otherwise = False

is_up_left_empty::GameState -> Coord -> Bool
is_up_left_empty s (x,y)
 |not((x-1, y-1) `elem` s^.redPieces) &&
  not((x-1, y-1) `elem` s^.redKings) &&
  not((x-1, y-1) `elem` s^.blackPieces) &&
  not((x-1, y-1) `elem` s^.blackKings) &&
  (x-1 >= 0)
  = True
 |otherwise = False

is_down_right_empty::GameState -> Coord -> Bool
is_down_right_empty s (x,y)
 |not((x+1, y+1) `elem` s^.redPieces) &&
  not((x+1, y+1) `elem` s^.redKings) &&
  not((x+1, y+1) `elem` s^.blackPieces) &&
  not((x+1, y+1) `elem` s^.blackKings) &&
  (x+1 <= 7)
  = True
 |otherwise = False

is_down_left_empty::GameState -> Coord -> Bool
is_down_left_empty s (x,y)
 |not((x-1, y+1) `elem` s^.redPieces) &&
  not((x-1, y+1) `elem` s^.redKings) &&
  not((x-1, y+1) `elem` s^.blackPieces) &&
  not((x-1, y+1) `elem` s^.blackKings) &&
  (x-1 >= 0)
  = True
 |otherwise = False
------------------------------------------


--DIAGONAL JUMP HELPERS-------------------

opponent_occupied::Coord->GameState->Bool
opponent_occupied (x,y) s
 | s^.status == Red
 = if(((x, y) `elem` s^.blackPieces) || ((x, y) `elem` s^.blackKings))
    then True
    else False
 |s^.status == Black
 = if(((x, y) `elem` s^.redPieces) || ((x, y) `elem` s^.redKings))
    then True
    else False
 |otherwise = False

notoccupied::Coord->GameState->Bool
notoccupied (x,y) s 
 | s^.status == Red
 = if(((x, y) `elem` s^.blackPieces) || ((x, y) `elem` s^.blackKings) || ((x, y) `elem` s^.redPieces) || ((x, y) `elem` s^.redKings))
    then  False
    else  True
 |s^.status == Black
 = if(((x, y) `elem` s^.redPieces) || ((x, y) `elem` s^.redKings) || ((x, y) `elem` s^.blackPieces) || ((x, y) `elem` s^.blackKings))
    then  False
    else  True
 |otherwise = False

onboard::Coord->Bool
onboard (x,y)
 |(x>=0) && (x<=7) && (y<=7) && (x>=0)
  = True
 | otherwise = False

is_up_right_jump_possible::GameState-> Coord ->Bool
is_up_right_jump_possible s (x,y)
 |(s^.status == Red) &&
  not((x+2, y-2) `elem` s^.redPieces) &&
  not((x+2, y-2) `elem` s^.redKings) &&
  not((x+2, y-2) `elem` s^.blackPieces) &&
  not((x+2, y-2) `elem` s^.blackKings) &&
  (x+2 <= 7) && (y-2 >= 0) &&
  ((x+1, y-1) `elem` (s^.blackPieces ) || (x+1, y-1) `elem` (s^.blackKings ))
  = True
  |(s^.status == Black) &&
  not((x+2, y-2) `elem` s^.redPieces) &&
  not((x+2, y-2) `elem` s^.redKings) &&
  not((x+2, y-2) `elem` s^.blackPieces) &&
  not((x+2, y-2) `elem` s^.blackKings) &&
  (x+2 <= 7) && (y-2 >= 0) &&
  ((x+1, y-1) `elem` (s^.redPieces ) || (x+1, y-1) `elem` (s^.redKings ))
  = True
  |otherwise = False

is_up_left_jump_possible::GameState-> Coord ->Bool
is_up_left_jump_possible s (x,y)
 |(s^.status == Red) && 
  not((x-2, y-2) `elem` s^.redPieces) &&
  not((x-2, y-2) `elem` s^.redKings) &&
  not((x-2, y-2) `elem` s^.blackPieces) &&
  not((x-2, y-2) `elem` s^.blackKings) &&
  (x-2 >= 0) && (y-2 >= 0) &&
  ((x-1, y-1) `elem` (s^.blackPieces ) || (x-1, y-1) `elem` (s^.blackKings ))
  = True
 |(s^.status == Black) && 
  not((x-2, y-2) `elem` s^.redPieces) &&
  not((x-2, y-2) `elem` s^.redKings) &&
  not((x-2, y-2) `elem` s^.blackPieces) &&
  not((x-2, y-2) `elem` s^.blackKings) &&
  (x-2 >= 0) && (y-2 >= 0) &&
  ((x-1, y-1) `elem` (s^.redPieces ) || (x-1, y-1) `elem` (s^.redKings ))
  = True
  |otherwise = False
  
is_down_left_jump_possible::GameState-> Coord ->Bool
is_down_left_jump_possible s (x,y)
 |(s^.status == Red) && 
  not((x-2, y+2) `elem` s^.redPieces) &&
  not((x-2, y+2) `elem` s^.redKings) &&
  not((x-2, y+2) `elem` s^.blackPieces) &&
  not((x-2, y+2) `elem` s^.blackKings) &&
  (x-2 >= 0) && (y+2 <= 7) &&
  ((x-1, y+1) `elem` (s^.blackPieces ) || (x-1, y+1) `elem` (s^.blackKings ))
  = True
 |(s^.status == Black) && 
  not((x-2, y+2) `elem` s^.redPieces) &&
  not((x-2, y+2) `elem` s^.redKings) &&
  not((x-2, y+2) `elem` s^.blackPieces) &&
  not((x-2, y+2) `elem` s^.blackKings) &&
  (x-2 >= 0) && (y+2 <= 7) &&
  ((x-1, y+1) `elem` (s^.redPieces ) || (x-1, y+1) `elem` (s^.redKings ))
  = True
  |otherwise = False

is_down_right_jump_possible::GameState-> Coord ->Bool
is_down_right_jump_possible s (x,y)
 |(s^.status == Red) &&
  not((x+2, y+2) `elem` s^.redPieces) &&
  not((x+2, y+2) `elem` s^.redKings) &&
  not((x+2, y+2) `elem` s^.blackPieces) &&
  not((x+2, y+2) `elem` s^.blackKings) &&
  (x+2 <= 7) && (y+2 <= 7) &&
  ((x+1, y+1) `elem` (s^.blackPieces ) || (x+1, y+1) `elem` (s^.blackKings ))
  = True
  |(s^.status == Black) &&
  not((x+2, y+2) `elem` s^.redPieces) &&
  not((x+2, y+2) `elem` s^.redKings) &&
  not((x+2, y+2) `elem` s^.blackPieces) &&
  not((x+2, y+2) `elem` s^.blackKings) &&
  (x+2 <= 7) && (y+2 <= 7) &&
  ((x+1, y+1) `elem` (s^.redPieces ) || (x+1, y+1) `elem` (s^.redKings ))
  = True
  |otherwise = False
------------------------------------------
removePiece:: [Coord] -> Coord -> [Coord]
removePiece [] _  = []
removePiece l c =  filter (\r ->  compareCoord r c ) l


removeComponent::GameState->Coord->Coord->GameState
removeComponent s (x,y) (x2,y2)
 |_status s == Red
 = s{_blackPieces = removePiece (_blackPieces s) (mid_jump_coord (x,y) (x2,y2))
   , _redPieces = removePiece (_redPieces s) (x,y)
   , _redKings  = removePiece (_redKings s)  (x,y)
   , _blackKings =  removePiece (_blackKings s) (mid_jump_coord (x,y) (x2,y2))
   }
 |_status s == Black
  = s{_redPieces =removePiece (_redPieces s) (mid_jump_coord (x,y) (x2,y2))
   , _blackPieces = removePiece (_blackPieces s) (x,y)
   , _blackKings  = removePiece (_blackKings s)  (x,y)
   , _redKings =  removePiece (_redKings s) (mid_jump_coord (x,y) (x2,y2))
   }
  | otherwise = s
--Movement helper functions-------------------------------
apply_simple_move::GameState->Move->GameState
apply_simple_move s m = case s^.status of
 Red-> if((head(tail m)) `elem` firstRow)
        then setMessage $ set status Black $ set redPieces ((removePiece (s^.redPieces) (head m))) $ set redKings ((head(tail m)):(s^.redKings)) s
        else setMessage $ set status Black $ set redPieces ((head(tail m)):(removePiece (s^.redPieces) (head m))) s
 Black->if((head(tail m)) `elem` lastRow)
        then setMessage $ set status Red $ set blackPieces ((removePiece (s^.blackPieces) (head m))) $ set blackKings ((head(tail m)):(s^.blackKings)) s
        else setMessage $ set status Red $ set blackPieces ((head(tail m)):(removePiece (s^.blackPieces) (head m))) s

applyK_simple_move::GameState->Move->GameState
applyK_simple_move s m = case s^.status of
 Red->setMessage $ set status Black $ set redKings ((head(tail m)):(removePiece (s^.redKings) (head m))) s
 Black-> setMessage $ set status Red $ set blackKings ((head(tail m)):(removePiece (s^.blackKings) (head m))) s

apply_jump_move::Move->GameState->GameState
apply_jump_move [] s = s
apply_jump_move ((x,y):[]) s = s
apply_jump_move ((x,y):(x2,y2):ms) s
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && not(((x2,y2) `elem` firstRow))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces ((x2,y2):(removePiece (s^.redPieces) (x,y) )) s
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && ((x2,y2) `elem` firstRow)
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces ((removePiece (s^.redPieces) (x,y) )) $ set redKings ((x2,y2):(removePiece (_redKings s) (x,y))) s
 | ((_status s) == Red) && ((x,y) `elem` (_redKings s))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redKings ((x2,y2):(removePiece (s^.redKings) (x,y) )) s
 
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && not(((x2,y2) `elem` lastRow))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces ((x2,y2):(removePiece (s^.blackPieces) (x,y) )) s
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && ((x2,y2) `elem` lastRow)
  = setMessage $ set status  Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces ((removePiece (s^.blackPieces) (x,y) )) $ set blackKings ((x2,y2):(removePiece (_blackKings s) (x,y))) s
 | ((_status s) == Black) && ((x,y) `elem` (_blackKings s))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackKings ((x2,y2):(removePiece (s^.blackKings) (x,y) )) s
 |otherwise =  apply_jump_move [] s
---------------------------------------

mid_jump_coord::Coord->Coord->Coord
mid_jump_coord (x,y) (x2,y2) = (((x+x2)`div` 2),((y+y2)`div`2))


toogle_status::GameState->GameState
toogle_status s
 |((not(is_there (_redPieces s))) && (not(is_there (_redKings s)))) || (not(is_there (moves s))) = setMessage $ set status GameOver s
 |((not(is_there (_blackPieces s))) && (not(is_there (_blackKings s)))) || (not(is_there (moves s))) = setMessage $ set status GameOver s
 | otherwise = s


firstRow = [(0,0), (1,0),(2,0), (3,0),(4,0) ,(5,0),(6,0), (7,0)]
lastRow  = [ (0,7), (1,7),(2,7),(3,7), (4,7), (5,7),(6,7), (7,7)]
compareCoord:: Coord-> Coord -> Bool
compareCoord (x,y) (m,p) 
 | x==m && y==p = False
 | otherwise = True

is_there::[a]->Bool
is_there [] = False
is_there ms = True


isOdd :: Coord -> Bool
isOdd (x,y) = (mod (x+y) 2) == 0

--s^.blackPieces
--_blackPieces s

--set blackPieces [] $
--set redPieces [] s
{-
s{_blackPieces = []
 ,_redPieces = []
 ,_message = "hey" s}
-}

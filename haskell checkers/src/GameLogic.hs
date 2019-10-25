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


initialGameState :: GameState
initialGameState =
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = [(4,3)]
            , _redKings = [(3,4)]
            , _status = Red
            , _message = "Red Turn."}
{-
blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]
-}
blackInit :: [Coord]
blackInit = [ (1,0)]

redInit :: [Coord]
redInit = [ (0,7)]
setMessage :: GameState -> GameState
setMessage s = case (s^.status) of
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
         then if ((head m)`elem`(s^.redPieces))
               then toogle_status $ apply_jump_move m s
               else toogle_status $ apply_jump_move m s
        else set message "Invalid move" s
 Black -> if ((islegal s m)&&(m `elem` (simple_moves s)) &&(is_there (jump_moves s)))
           then set message "There is a jump move available" s
           else if ((islegal s m)&&(m `elem` (simple_moves s)) && (not(is_there (jump_moves s)))) 
           then if ((head m)`elem`(s^.blackPieces)) 
               then apply_simple_move s m
               else applyK_simple_move s m
          else if ((islegal s m)&&(m `elem` (jump_moves s)))
           then if ((head m)`elem`(s^.blackPieces))
               then apply_jump_move m s
               else apply_jump_move m s
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
 
--all possible  moves for red kings
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
 
--all possible blakc king moves
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
jump_moves::GameState -> [Move]
jump_moves s = case s^.status of
 Red -> red_jump_moves s
 Black -> black_jump_moves s
 _ -> []

red_jump_moves::GameState->[Move]
red_jump_moves s = (rsimple_jumps s (s^.redPieces) []) ++ (rking_jumps s (s^.redKings) [])

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

rking_jumps::GameState->[Coord]->[Move]->[Move]
rking_jumps s [] m = m
rking_jumps s ((x,y):rest) m = m
-- dont fall of the board
-- you want a piece of the other color diagonally, free space after that
-- Right jump for red pawn
-- we have a red pawn in (x,y) black pawn in ( x+1, y-1) and a free spot in (x+2, y-2)
black_jump_moves::GameState->[Move]
black_jump_moves s = (bsimple_jumps s (s^.blackPieces) []) ++ (bking_jumps s (s^.blackKings) [])

bsimple_jumps::GameState->[Coord]->[Move]->[Move]
bsimple_jumps s [] m = m
bsimple_jumps s ((x,y):rest) m = m

bking_jumps::GameState->[Coord]->[Move]->[Move]
bking_jumps s [] m = m
bking_jumps s ((x,y):rest) m = m

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

------------------------------------------
removePiece:: [Coord] -> Coord -> [Coord]
removePiece [] _  = []
removePiece l c =  filter (\r ->  compareCoord r c ) l

--Movement helper functions-------------------------------
apply_simple_move::GameState->Move->GameState
apply_simple_move s m = case s^.status of
 Red-> if((head(tail m)) `elem` firstRow)
        then setMessage $ set status Black $ set redPieces ((removePiece (s^.redPieces) (head m))) $ set redKings ((head(tail m)):(s^.redKings)) s
        else setMessage $ set status Black $ set redPieces ((head(tail m)):(removePiece (s^.redPieces) (head m))) s
 Black->if((head(tail m)) `elem` lastRow)
        then setMessage $ set status Red $ set blackPieces ((removePiece (s^.blackPieces) (head m))) $ set blackKings ((head(tail m)):(s^.redKings)) s
        else setMessage $ set status Red $ set blackPieces ((head(tail m)):(removePiece (s^.blackPieces) (head m))) s

applyK_simple_move::GameState->Move->GameState
applyK_simple_move s m = case s^.status of
 Red->setMessage $ set status Black $ set redKings ((head(tail m)):(removePiece (s^.redKings) (head m))) s
 Black-> setMessage $ set status Red $ set blackKings ((head(tail m)):(removePiece (s^.blackKings) (head m))) s

apply_jump_move::Move->GameState->GameState
apply_jump_move [] s = s
apply_jump_move ((x,y):[]) s = s
apply_jump_move ((x,y):(x2,y2):ms) s
 | (s^.status == Red) && ((x,y) `elem` (s^.redPieces))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces ((x2,y2):(removePiece (s^.redPieces) (x,y) )) s
 | (s^.status == Red) && ((x,y) `elem` (s^.redKings))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redKings ((x2,y2):(removePiece (s^.redKings) (x,y) )) s
 | (s^.status == Black) && ((x,y) `elem` (s^.blackPieces))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces ((x2,y2):(removePiece (s^.redKings) (x,y) )) s
 | (s^.status == Black) && ((x,y) `elem` (s^.blackKings))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackKings ((x2,y2):(removePiece (s^.redKings) (x,y) )) s
 |otherwise =  apply_jump_move [] s
---------------------------------------

mid_jump_coord::Coord->Coord->Coord
mid_jump_coord (x,y) (x2,y2) = (((x+x2)`div` 2),((y+y2)`div`2))


toogle_status::GameState->GameState
toogle_status s
 |((not(is_there (s^.redPieces))) && (not(is_there (s^.redKings)))) = setMessage $ set status GameOver s
 |((not(is_there (s^.blackPieces))) && (not(is_there (s^.blackKings)))) = setMessage $ set status GameOver s
 | otherwise = s


firstRow = [ (1,0), (3,0), (5,0), (7,0)]
lastRow  = [ (0,7), (2,7), (4,7), (6,7)]
compareCoord:: Coord-> Coord -> Bool
compareCoord (x,y) (m,p) 
 | x==m && y==p = False
 | otherwise = True

is_there::[a]->Bool
is_there [] = False
is_there ms = True


isOdd :: Coord -> Bool
isOdd (x,y) = (mod (x+y) 2) == 0


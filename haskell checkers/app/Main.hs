module Main where

--import Tui
import Checkers
--import GameLogic
import Lens.Micro.Platform
import Moves
import GameLogic

main :: IO ()
--main = human applyTHEMove initialGameState
--main = moves initialGameState
main = tui
 {-
applyTHEMove :: Move -> GameState -> GameState
applyTHEMove m s = case s^.status of
 Red -> if ((islegal s m)&&(m `elem` (simple_moves s)) &&(is_there (jump_moves s)))
         then set message "There is a jump move available" s
         else if ((islegal s m)&&(m `elem` (simple_moves s)) && (not(is_there (jump_moves s)))) 
         then if ((head m)`elem`(_redPieces s)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
         else if ((islegal s m)&&(m `elem` (jump_moves s)))
            then if ((head m)`elem`(_redPieces s) || (head m)`elem`(_redKings s))
                  then toogle_status $ apply_jump_move m s
                  else toogle_status $ apply_jump_move m s
        else set message "Invalid move" s
 Black -> if ((islegal s m)&&(m `elem` (simple_moves s)) &&(is_there (jump_moves s)))
           then set message "There is a jump move available" s
           else if ((islegal s m)&&(m `elem` (simple_moves s)) && (not(is_there (jump_moves s)))) 
           then if ((head m)`elem`(_blackPieces s)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
          else if ((islegal s m)&&(m `elem` (jump_moves s)))
           then if ((head m)`elem`(_blackPieces s) || (head m)`elem`(_blackKings s))
               then toogle_status $ apply_jump_move m s
               else toogle_status $ apply_jump_move m s
        else set message "Invalid move" s
 _ -> initialGameState


 















{-
islegal::GameState->Move->Bool
islegal s [] = False
islegal s m
 | m `elem` (moves s)  = True 
 | otherwise = False
-}








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


-}

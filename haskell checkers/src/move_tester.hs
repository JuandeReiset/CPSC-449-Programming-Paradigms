module Main where

import Moves
import GameStructures


g1 = GameState { _blackPieces = [(4,1),(2,1)]
            , _redPieces = []
            , _blackKings = []
            , _redKings = [(5,0)]
            , _status = Red
            , _message = ""}

 

g2 = GameState { _blackPieces = [(6,1),(4,1),(2,1)]
            , _redPieces = []
            , _blackKings = []
            , _redKings = [(7,2)]
            , _status = Red
            , _message = ""}

 

g3 =  GameState { _blackPieces = [(6,3),(6,1),(4,1),(2,1)]
            , _redPieces = []
            , _blackKings = []
            , _redKings = [(5,4)]
            , _status = Red
            , _message = ""}

 

g4 =  GameState { _blackPieces = [(4,3),(6,3),(6,1),(4,1),(2,1)]
            , _redPieces = []
            , _blackKings = []
            , _redKings = [(3,2)]
            , _status = Red
            , _message = ""}

 

-- Black move to test absearch

g5 = GameState { _blackPieces = [(7,0)]
            , _redPieces = [(4,5),(6,5)]
            , _blackKings = [(4,7)]
            , _redKings = [(3,0)]
            , _status = Black
            , _message = ""}

 

--Red move  move to test absearch

g6 = GameState { _blackPieces = [(7,0)]
            , _redPieces = [(4,5),(6,5),(4,3),(6,3)]
            , _blackKings = [(5,6)]
            , _redKings = [(3,0)]
            , _status = Red
            , _message = ""}

 

-- red move to win!

g7 = GameState { _blackPieces = [(7,6)]
            , _redPieces = []
            , _blackKings = [(0,7)]
            , _redKings = [(1,4),(6,7)]
            , _status = Red
            , _message = ""}

 

t1Black = GameState{ _blackPieces = [(3,3),(5,3),(5,5)]
            , _redPieces = [(4,4)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

t2Black = GameState{ _blackPieces = [(3,3),(5,3),(5,5)]
            , _redPieces = [(4,4), (2,6)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

t3Black = GameState{ _blackPieces = [(6,6)]
            , _redPieces = [(7,7)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

t4Black = GameState{ _blackPieces = [(3,3),(5,3),(5,5)]
            , _redPieces = [(4,4), (2,6),(4,6),(6,6)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

-- Pawn changing to King

t5Black = GameState{ _blackPieces = [(4,6)]
            , _redPieces = [(3,3)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

--non-loopy jumps

t6Black = GameState{ _blackPieces = [(3,3),(5,3),(5,5)]
            , _redPieces = [(4,4), (2,6),(6,6)]
            , _blackKings = []
            , _redKings = []
            , _status = Black
            , _message = ""}

 

--Initial Game GameState

t7Black = initialGameState

 

moves_tester [] = putStrLn ""
moves_tester ((name,st,ans):rest) =

      do putStrLn ""
         putStrLn ("Testing "++name++":")
         putStrLn ("     "++ (show (moves st)))
         putStrLn "Should be:"
         putStrLn  ans
         putStrLn ""
         moves_tester rest

 

main = moves_tester [("g1",g1,"[[(5,0),(3,2),(1,0)]]")
                    ,("g2",g2,"[[(7,2),(5,0),(3,2),(1,0)]]")
                    ,("g3",g3,"[[(5,4),(7,2),(5,0),(3,2),(1,0)]]")
                    ,("g4",g4,"[[(3,2),(5,4),(7,2),(5,0),(3,2),(1,0)],[(3,2),(5,0),(7,2),(5,4),(3,2),(1,0)],[(3,2),(1,0)]]")
                    ,("g5",g5,"[[(4,7),(5,6)],[(4,7),(3,6)],[(7,0),(6,1)]]")
                    ,("g6",g6,"[[(3,0),(4,1)],[(3,0),(2,1)],[(4,5),(5,4)],[(4,5),(3,4)],[(6,5),(7,4)],[(6,5),(5,4)],[(4,3),(5,2)],[(4,3),(3,2)],[(6,3),(7,2)],[(6,3),(5,2)]]")
                    ,("g7",g7,"[[(1,4),(2,5)],[(1,4),(0,5)],[(1,4),(2,3)],[(1,4),(0,3)],[(6,7),(5,6)]]")
                    ,("t1Black",t1Black,"[[(5,3),(3,5)]]")
                    ,("t2Black",t2Black,"[[(5,3),(3,5),(1,7)]]")
                    ,("t3Black",t3Black,"[[(6,6),(5,7)]]")
                    ,("t4Black",t4Black,"[[(5,3),(3,5),(5,7),(7,5)],[(5,3),(3,5),(1,7)],[(5,5),(7,7)],[(5,5),(3,7),(1,5)]]")
                    ,("t5Black",t5Black,"[[(4,6),(5,7)],[(4,6),(3,7)]]")
                    ,("t6Black",t6Black,"[[(5,3),(3,5),(1,7)],[(5,5),(7,7)]]")
                    ,("t7Black",t7Black,"[[(0,5),(1,4)],[(2,5),(3,4)],[(2,5),(1,4)],[(4,5),(5,4)],[(4,5),(3,4)],[(6,5),(7,4)],[(6,5),(5,4)]]")]

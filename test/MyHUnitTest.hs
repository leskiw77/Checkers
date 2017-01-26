module MyHUnitTest where

import Game
import ChessBoard
import Elements

import Test.HUnit



tests = TestList [
                  "test simple move" ~: b_board2 ~=? (mkMove b_initialBoard (2,3) (3,2) Simple),
                  "test move with taking rivals piece" ~: b_board4 ~=? (mkMove b_board3 (5,2) (7,4) Kill),
                  "coordinates match piece color" ~: assertBool "" (colorMatch b_board2 White (8,1)),
                  "coordinates match piece color" ~: assertBool "" (not $ colorMatch b_board3 Black (4,4)),
                  "has winner" ~: (hasWinner b_board5) ~=? WhitePl,
                  "has winner" ~: (hasWinner b_board6) ~=? None,
                  "has winner" ~: (hasWinner b_board7) ~=? BlackPl,
                  "all possibilities" ~: allPossibilities b_initialBoard BlackPl (2,1) ~=? ([(3,2)],[]),
                  "all possibilities" ~: allPossibilities b_initialBoard WhitePl (8,2) ~=? ([],[]),
                  "all possibilities" ~: allPossibilities b_board3 BlackPl (5,2) ~=? ([(6,1)],[(7,4)]),
                  "maping piece to player color" ~: (mapPieceToPlayer White) ~=? (WhitePl),
                  "maping piece to player color" ~: (mapPieceToPlayer Black) ~=? (BlackPl),
                  "maping player color to piece" ~: (mapPlayerToPiece BlackPl) ~=? (Black),
                  "maping player color to piece" ~: (mapPlayerToPiece WhitePl) ~=? (White),
                  "convert square into character" ~: showSquare Nothing ~=? (' '),
                  "convert square into character" ~: showSquare (Just Black) ~=? ('X')
                 ]


testH = do
  runTestTT $ tests

initialBoardStr =  [ " X X X X"
                    ,"X X X X "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ," O O O O"
                    ,"O O O O "
                   ]
b_initialBoard = readBoard initialBoardStr


board2           =  [" X X X X"
                    ,"X   X X "
                    ," X      "
                    ,"        "
                    ,"        "
                    ,"        "
                    ," O O O O"
                    ,"O O O O "
                   ]

b_board2 = readBoard board2

board3           =  [" X X X X"
                    ,"X X X X "
                    ,"        "
                    ,"        "
                    ," X      "
                    ,"  O     "
                    ," O   O O"
                    ,"O O O O "
                   ]

b_board3 = readBoard board3

board4           =  [" X X X X"
                    ,"X X X X "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ," O X O O"
                    ,"O O O O "
                   ]

b_board4 = readBoard board4

board5           =  ["     O  "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"  O     "
                    ," O   O O"
                    ,"        "
                   ]

b_board5 = readBoard board5

board6           =  ["        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"  X     "
                    ,"   O    "
                    ,"        "
                   ]


b_board6 = readBoard board6

board7           =  ["        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"   X    "
                    ,"        "
                   ]
b_board7 = readBoard board7

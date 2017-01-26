module Game
  (play)
where

import ChessBoard
import Elements
import Input

turn :: Board -> Player -> IO ()
turn board player = do
  showBoard board
  putStrLn $ "Ruch mają "++ showPl player
  -- TODO: catch exception
  (from,to,mode) <- getCords board player
  let newBoard = mkMove board from to mode
  if (hasWinner newBoard) /= None
  then
     putStrLn $ "Zwyciezca sa " ++ showPl (hasWinner board)
      else
        turn newBoard (inverse player)

-- | Start game. Use standart initial board. White player begin
play :: IO ()
play = turn (readBoard initialBoardStr) WhitePl

initialBoardStr =  [ " X X X X"
                    ,"X X X X "
                    ,"        "
                    ,"        "
                    ,"        "
                    ,"        "
                    ," O O O O"
                    ,"O O O O "
                    ]

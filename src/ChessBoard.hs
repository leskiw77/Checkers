module ChessBoard
  (
  Board,
  showBoard,
  mkMove,
  insideBoard,
  hasWinner,
  colorMatch,
  allPossibilities,
  readBoard
  )

where

import Elements

type Board = [[Square]]

-- | Inform  whether piece color match position.
colorMatch :: Board ->  Piece -- ^  color of the player to be moved
                    -> (Int, Int) -- ^ coordinates
                    ->Bool
colorMatch board  piece (x,y)=
  takeElem2D x y (toStringBoard board) == showPiece piece


isEmptySquare :: Board -> (Int, Int) -> Bool
isEmptySquare board (x,y) =
  takeElem2D x y (toStringBoard board) == ' '

-- | Are coordinates in tuple in corect. Propoper range between 1 and 8
insideBoard :: (Int, Int) -> Bool
insideBoard (x,y) =
  (x>0) && (x<9) && (y>0) && (y<9)


-- | Function check if rivals piece is in the middle of the cords
canTakePiece :: Board -> Player -- ^  color of the player to be moved
                      -> (Int, Int) -- ^ start coordinates
                      -> (Int, Int) -- ^ attempt cords
                      -> Bool
canTakePiece board player (x1,y1) (x2,y2) = do
  let takenX = (min x1 x2) + 1
  let takenY = (min y1 y2) + 1
  colorMatch board (mapPlayerToPiece $ inverse player) (takenX,takenY)


generatePossibilities :: Player -> (Int,Int) -> [(Int,Int)]
generatePossibilities player (x,y) =
  case player of
    WhitePl -> filter insideBoard [(x-1,y+1),(x-1,y-1),(x-2,y-2),(x-2,y+2)]
    BlackPl -> filter insideBoard [(x+1,y+1),(x+1,y-1),(x+2,y-2),(x+2,y+2)]

generateSimpleMove :: Board -> Player -> (Int, Int) -> [(Int, Int)]
generateSimpleMove board player (x,y) = do
  let f = (\x -> insideBoard (x) &&  isEmptySquare board (x))
  case player of
    WhitePl -> filter f [(x-1,y+1),(x-1,y-1)]
    BlackPl -> filter f [(x+1,y+1),(x+1,y-1)]

generateMoveWithKill :: Board -> Player -> (Int, Int) -> [(Int, Int)]
generateMoveWithKill board player (x,y) = do
  let f = (\z -> insideBoard (z) && isEmptySquare board (z) && canTakePiece board player (x,y) z)
  case player of
    WhitePl -> filter f [(x-2,y+2),(x-2,y-2)]
    BlackPl -> filter f [(x+2,y+2),(x+2,y-2)]


-- | Function ganerates all possible moves for piece. In first argument the simple moves whould be placed (that mean without taking any piece)
-- | and in second argument moves which take rivals piece are placed.
-- | Lenght of each list in tuple vary between 0 and 2
allPossibilities :: Board -> Player -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
allPossibilities board player (x,y) = do
  if insideBoard (x,y) && colorMatch board (mapPlayerToPiece player) (x,y)
    then ((generateSimpleMove board player (x,y)), (generateMoveWithKill board player (x,y)))
      else
        ([],[])

canTakeAny :: Board -> Player -> (Int, Int) -> Bool
canTakeAny board player cord = do
  let (_,x) = allPossibilities board player cord
  x /= []

takeElem :: Int -> [a] -> a
takeElem x xs =last $ take x xs

takeElem2D :: Int -> Int -> [[a]] -> a
takeElem2D x y matrix = takeElem y $ takeElem x matrix

exchangeElement :: [a] -> Int -> a -> [a]
exchangeElement list x val = do
  let (xs,_:ys) = splitAt (x-1) list
  xs ++ [val] ++ ys

exchangeElement2D :: [[a]] -> Int -> Int -> a -> [[a]]
exchangeElement2D list x y val = do
  let excg = takeElem x list --take row to exchange
  let newRow = exchangeElement excg y val -- create changed row
  exchangeElement list x newRow

move :: Board -> (Int, Int) -> (Int, Int) -> Board
move board (x1,y1) (x2,y2) = do
    let brdstr = toStringBoard board
    let fir = takeElem2D x1 y1 brdstr
    let sec = takeElem2D x2 y2 brdstr
    let brdstr' = exchangeElement2D brdstr x1 y1  sec
    let brdstr'' = exchangeElement2D brdstr' x2 y2 fir
    readBoard brdstr''


-- | Change the position of piece. In Kill mode enemies piece is being taken. No validation inside.
mkMove :: Board -> (Int, Int) -- ^ begin cords
                -> (Int, Int) -- ^ final cords
                -> Mode -- ^ with removing enemy or not
                -> Board
mkMove board (x1,y1) (x2,y2) mode = do
  let newboard = move board (x1,y1) (x2,y2)
  if mode == Kill
    then
      readBoard $ exchangeElement2D (toStringBoard newboard) ((min x1 x2) + 1) ((min y1 y2) + 1) ' '
        else
          newboard



whitePlayerWon:: Board -> Bool
whitePlayerWon board = not $ foldl (||) False $ map (elem 'X') (toStringBoard board)

blackPlayerWon:: Board -> Bool
blackPlayerWon board = not $ foldl (||) False $ map (elem 'O') (toStringBoard board)

-- | Indicates if game have a winner and return its color. If don't gives None
hasWinner::Board -> Player
hasWinner board = do
  if whitePlayerWon board
    then WhitePl
      else if blackPlayerWon board
            then BlackPl
              else None


-- | Produce type board from list of String
readBoard :: [String] -- ^ Take list of String which should contain only 'X','O'(o not zero) or ' ' (space)
              -> Board
readBoard b = map (map readSquare) b

toStringBoard :: Board -> [String]
toStringBoard b = map (map showSquare) b

-- | Present whole board with coordinates
showBoard :: Board -> IO()
showBoard b = putStrLn "\n   ABCDEFGH" >> (showBoardRec b 8)

showBoardRec:: Board -> Int -> IO()
showBoardRec _ 0 = putStrLn "   ABCDEFGH\n"
showBoardRec b num = do
  putStrLn $ show num ++ "| " ++ takeElem (9-num)(toStringBoard b) ++ "| "
  showBoardRec b (num-1)

module Elements
  (
  Square,
  Piece(White, Black),
  Player(WhitePl,BlackPl,None),
  Mode(Simple,Kill),
  mapPieceToPlayer,
  mapPlayerToPiece,
  showSquare,
  readSquare,
  showPiece,
  readPiece,
  inverse,
  showPl
  )

where

type Square = Maybe Piece
data Piece = White | Black  deriving (Show,Eq)
data Player = WhitePl | BlackPl | None deriving(Show,Eq)
data Mode = Simple | Kill deriving (Show,   Eq)

-- | Convert piece color into players color
mapPieceToPlayer :: Piece -> Player
mapPieceToPlayer White = WhitePl
mapPieceToPlayer Black = BlackPl

-- | Convert player color into piece color
mapPlayerToPiece :: Player -> Piece
mapPlayerToPiece WhitePl = White
mapPlayerToPiece BlackPl = Black
mapPlayerToPiece None = error("Nie mozna mapować gracza None na jakiś kolor pionka")

-- | Convert square into character
showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

-- | Convert character into square
readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c   = Just (readPiece c)

-- | Convert piece into character
showPiece :: Piece -> Char
showPiece White = 'O'
showPiece Black = 'X'

-- | Convert character into piece
readPiece :: Char -> Piece
readPiece 'O' = White
readPiece 'X' = Black

-- | Inverse player color
inverse::Player->Player
inverse WhitePl = BlackPl
inverse BlackPl = WhitePl

-- | Convert player into polish name: White into "Biali" and Black into "Czarni"
showPl::Player -> String
showPl WhitePl = "Biali"
showPl BlackPl = "Czarni"

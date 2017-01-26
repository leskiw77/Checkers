module Input
  (
  getCords
  )
where

import Elements
import ChessBoard

import Data.List
import Data.Char
import Data.Maybe
import Control.Exception

data MyException = MkMyException deriving Show
instance Exception MyException

mapAlphaToNum :: Char -> Maybe Int
mapAlphaToNum c =
  (+1) <$> (elemIndex c ['A'..'H'])

firstWord :: [Char] -> [Char]
firstWord line =
  map Data.Char.toUpper $ takeWhile (/=' ') $ dropWhile (==' ') line

takeCoordinates :: [Char] -> (Int, Int)
takeCoordinates line = do
  let word = map toUpper ( filter (/=' ') line )
  let s = fromJust $ mapAlphaToNum $ head word --throws exception if not match
  let f = digitToInt $ head $ firstWord $ tail word
  if s<1 || s>8
    then
      throw MkMyException
    else
      (9-f,s)

parseFrom :: Board -> [Char] -> Player -> (Int, Int)
parseFrom board line player = do
  let result = takeCoordinates line
  if colorMatch board (mapPlayerToPiece player) result
    then result
      else throw MkMyException


parseTo:: Board -> [Char] -> Player -> (Int, Int) -> ((Int, Int), (Int, Int), Mode)
parseTo board line player from = do
  let to = takeCoordinates line
  if elem to (fst $ allPossibilities board player from)
    then (from, to, Simple)
      else if elem to (snd $ allPossibilities board player from)
              then (from,to,Kill)
                else throw MkMyException

-- | Return coordinates of the move and mode (simple or with take rival piece)
-- | Throws exception if choose wrong piece or move is forbidden
getCords :: Board -> Player -> IO ((Int, Int), (Int, Int), Mode)
getCords board player = do
  putStrLn "Podaj ktorym pionkiem chcesz sie ruszyc (np. A7)"
  line <- getLine
  let from = parseFrom board line player
  putStrLn "Podaj gdzie chcesz sie ruszyc"
  line <- getLine
  let to = parseTo board line player from
  return to

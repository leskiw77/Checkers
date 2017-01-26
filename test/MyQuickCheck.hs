module MyQuickCheck where

import Game
import ChessBoard
import Elements

import Test.QuickCheck

instance Arbitrary Player where
  arbitrary = oneof [return WhitePl, return BlackPl]

prop_invertPlayer x = (inverse $ inverse x) == x

prop_insideBoard = forAll (choose ( 1, 8)) $ \ x -> forAll (choose ( 1, 8)) $ \ y ->
                     insideBoard (x, y)


testQ = do
  quickCheck prop_invertPlayer
  quickCheck  prop_insideBoard

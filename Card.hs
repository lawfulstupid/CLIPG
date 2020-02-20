{-# LANGUAGE LambdaCase #-}

module CLIPG.Card where

import CLIPG.Drawing

data Face = Ace | Two | Three | Four | Five | Six
   | Seven | Eight | Nine | Ten | Jack | Queen | King
   deriving (Eq, Show, Enum, Bounded)
data Suit = Spades | Hearts | Clubs | Diamonds
   deriving (Eq, Show, Enum, Bounded)
data Card = Joker | Face `Of` Suit
   deriving (Eq)

instance Show Card where
   show Joker = "Joker"
   show (x `Of` y) = show x ++ " of " ++ show y
   
cardToLines :: Char -> Char -> [String]
cardToLines f s =
   [ "┌───┐"
   , "│ " ++ [f] ++ " │"
   , "│ " ++ [s] ++ " │"
   , "└───┘" ]
-- ┌───┐
-- │ A │
-- │ ♠ │
-- └───┘

instance Render Card where
   build Joker = cardToLines '?' '¿'
   build (f `Of` s) = cardToLines (drawFace f) (drawSuit s)
      where
      drawFace = \case
         Ace -> 'A'
         Ten -> 'X'
         Jack -> 'J'
         Queen -> 'Q'
         King -> 'K'
         f -> head $ show (fromEnum f + 1)
      drawSuit = \case
         Spades -> '♠'
         Hearts -> '♥'
         Clubs -> '♣'
         Diamonds -> '♦'

data Card' = FaceDn Card | FaceUp Card

flipCard :: Card' -> Card'
flipCard (FaceDn card) = FaceUp card
flipCard (FaceUp card) = FaceDn card

instance Render Card' where
   build (FaceUp c) = build c
   build (FaceDn _) =
      [ "┌┬─┬┐"
      , "├╯ ╰┤"
      , "├╮ ╭┤"
      , "└┴─┴┘" ]
-- ┌┬─┬┐
-- ├╯ ╰┤
-- ├╮ ╭┤
-- └┴─┴┘
-- ╔╤═╤╕
-- ╟╯ ╰┤
-- ╟╮ ╭┤
-- ╙┴─┴┘
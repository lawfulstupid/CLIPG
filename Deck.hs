module Cards.Deck where

import Cards.Card
import AbLib.Data.Stack
import System.Random

type Deck = Stack Card

standard :: IO Deck
standard = fromList [face `Of` suit | suit <- [minBound..], face <- [minBound..]]

jokers :: Int -> IO Deck
jokers n = do
   std <- standard >>= toList
   fromList (replicate n Joker ++ std)

shuffle :: Deck -> IO ()
shuffle deck = do
   list  <- toList deck
   list' <- shuffle' list
   modifyStack (const list') deck
   where
   shuffle' [] = return []
   shuffle' deck = do
      r <- randomRIO (0, length deck - 1)
      let (a,c:b) = splitAt r deck
      fmap (c:) $ shuffle' (a++b)
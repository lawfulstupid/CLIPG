module CLIPG.Deck where

import CLIPG.Card
import CLIPG.Drawing

import AbLib.Data.Stack
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Data.Maybe (maybeToList, fromJust, isNothing)
import Control.Monad

newtype Deck = Deck (Stack Card')

instance Render Deck where
   build (Deck s) = unsafePerformIO (toList s >>= return . build')
      where
      build' [] =
         [ [curve (right & down), horizontal, horizontal, horizontal, curve (left & down)]
         , [vertical, ' ', ' ', ' ', vertical]
         , [vertical, ' ', ' ', ' ', vertical]
         , [curve (right & up), horizontal, horizontal, horizontal, curve (left & up)] ]
      build' ([c]) = build c
      build' (c:_) = zipWith (zipWith overlay) deckOverlay $ build c
         where
         deckOverlay = map (double <$>)
            [ [right & down, horizontal, horizontal, horizontal, left]
            , [vertical, ' ', ' ', ' ', ' ']
            , [vertical, ' ', ' ', ' ', ' ']
            , [up,       ' ', ' ', ' ', ' '] ]

standard52 :: IO Deck
standard52 = Deck <$> fromList [FaceDn (face `Of` suit) | suit <- [minBound..], face <- [minBound..]]

isEmpty :: Deck -> IO Bool
isEmpty (Deck s) = peek s >>= return . isNothing

flipTop :: Deck -> IO ()
flipTop deck = do
   x <- draw 1 deck
   unless (null x) $ putOnTop (flipCard $ head x) deck

flipDeck :: Deck -> IO ()
flipDeck deck = do
   x <- draw 1 deck
   unless (null x) $ do
      flipDeck deck
      putOnBottom (flipCard $ head x) deck

draw :: Int -> Deck -> IO [Card']
draw 0 _ = return []
draw 1 (Deck s) = pop s >>= return . maybeToList
draw n deck = do
   h <- draw 1 deck
   t <- draw (n-1) deck
   return (h++t)

putOnTop :: Card' -> Deck -> IO ()
putOnTop c (Deck s) = push c s

putOnBottom :: Card' -> Deck -> IO ()
putOnBottom c deck = do
   x <- draw 1 deck
   when   (null x) $ putOnTop c deck
   unless (null x) $ do
      putOnBottom c deck
      putOnTop (head x) deck

-- jokers :: Int -> IO Deck
-- jokers n = do
   -- std <- standard >>= toList
   -- fromList (replicate n Joker ++ std)

-- shuffle :: Deck -> IO ()
-- shuffle deck = do
   -- list  <- toList deck
   -- list' <- shuffle' list
   -- modifyStack (const list') deck
   -- where
   -- shuffle' [] = return []
   -- shuffle' deck = do
      -- r <- randomRIO (0, length deck - 1)
      -- let (a,c:b) = splitAt r deck
      -- fmap (c:) $ shuffle' (a++b)
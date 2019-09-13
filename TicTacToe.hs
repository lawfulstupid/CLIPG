{-# LANGUAGE LambdaCase #-}

-- run with `play Player` or `play Computer`

module CLIPG.TicTacToe where

import AbLib.Data.Grid
import Data.List (intercalate, minimumBy, maximumBy)
import Data.Ord (comparing)
import System.IO (getLine)
import Control.Applicative (liftA2)

data Player = Neither | Noughts | Crosses
   deriving (Eq)
   
showPlayer :: Player -> String
showPlayer = \case
   Noughts -> "Noughts"
   Crosses -> "Crosses"

opponent :: Player -> Player
opponent = \case
   Noughts -> Crosses
   Crosses -> Noughts
   Neither -> Neither

type Board = Grid Player

instance Show Player where
   show = \case
      Neither -> "   "
      Noughts -> " O "
      Crosses -> " X "

-- Nothing = no winner
checkWinner :: Board -> Maybe Player
checkWinner b = checkResults $ map checkRow $ (rows ++ cols ++ diag)
   where
   playersToMaybes = map $ \case
      Neither -> Nothing
      other   -> Just other
   rows = map playersToMaybes $ toList b
   cols = map playersToMaybes $ toList $ transpose b
   diag = map playersToMaybes $ case diagonals b of
      Nothing -> []
      Just (x,y) -> [x,y]
   
   -- Just Noughts | Just Crosses -> Just that
   -- No Nothings -> All Just Neither -> Just Neither
   -- Exists a Nothing -> Nothing
   checkResults :: [Maybe Player] -> Maybe Player
   checkResults = foldr f (Just Neither)
      where
      f (Just Neither) x = x
      f (Just another) _ = Just another
      f Nothing Nothing = Nothing
      f x y = f y x
      
   -- if they all match, Just [that]
   -- otherwise Nothing
   checkRow :: [Maybe Player] -> Maybe Player
   checkRow (c:r) = foldr (liftA2 $ \ x y -> if x == y then x else Neither) c r
   

type Move = (Int, Int)
type Score = Int

nextBestMove :: Board -> Player -> Move
nextBestMove = (fst .) . minimax
   where
   minimax :: Board -> Player -> (Move, Score)
   minimax b p = let
      spaces = getSpaces b
      scores = getScore p . flip (set b) p <$> spaces
      in if null spaces
         then errorWithoutStackTrace "no moves available"
         else maxScore $ zip spaces scores
   getScore :: Player -> Board -> Score
   getScore p b
      | checkWinner b == Just p = 1
      | checkWinner b == Just (opponent p) = -1
      | not (any (Neither ==) b) = 0
      | otherwise = negate . snd . minimax b $ opponent p
   maxScore :: [(Move,Score)] -> (Move,Score)
   maxScore (m:ms) = maxScore' m (m:ms)
   maxScore' :: (Move,Score) -> [(Move,Score)] -> (Move,Score)
   maxScore' m = \case
      [] -> m
      (m': _) | snd m' == 1 -> m'
      (m':ms) | snd m < snd m' -> maxScore' m' ms
      (_ :ms) -> maxScore' m ms

getSpaces :: Board -> [(Int,Int)]
getSpaces b = [p | p <- indices b, b #! p == Neither]

sample = Grid (3,3)
   [ [Neither, Crosses, Noughts]
   , [Crosses, Neither, Noughts]
   , [Neither, Noughts, Crosses] ]
   
initial = grid (3,3) Neither

data Agent = Player | Computer
   deriving (Eq)

play :: Agent -> IO ()
play agent = do
   putStrLn "You're playing Noughts, your opponent is Crosses."
   putStrLn "Noughts start."
   winner <- takeTurn Noughts initial
   case winner of
      Neither -> putStrLn "It's a draw!"
      other   -> putStrLn (showPlayer other ++ " wins!")
   where
   takeTurn :: Player -> Board -> IO Player
   takeTurn player board = do
      putStr $ show board
      let spaces = not (any (Neither ==) board)
      let winner = checkWinner board
      case winner of
         Just result -> return result
         _ -> do
            putStrLn (showPlayer player ++ "'s turn.")
            pos <- getMove player board
            takeTurn (opponent player) $ set board pos player
   getMove :: Player -> Board -> IO Move
   getMove = \case
      Noughts -> playerInput
      Crosses -> case agent of
         Player -> playerInput
         Computer -> return . flip nextBestMove Crosses
         

playerInput :: Board -> IO Move
playerInput board = do
   putStr "Pick a square: "
   sq <- read <$> getLine
   let pos = (mod (sq-1) 3, div (sq-1) 3)
   let rangeTest = 1 <= sq && sq <= 9
   let validTest = board # pos == Just Neither
   if rangeTest && validTest
      then return pos
      else playerInput board
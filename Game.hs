{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cards.Game where

import Cards.Card
import AbLib.Data.Stack
import Data.Map (Map)
import qualified Data.Map as Map

type Pile = Stack Card'

instance Draw Pile where
   build = const []

-- newGrid :: (Int, Int) -> Grid
-- newGrid (w,h) = Grid w h 

-- (!) :: Grid -> (Int, Int) -> Maybe Pile
-- (!) g (x,y) = g !! y !! x



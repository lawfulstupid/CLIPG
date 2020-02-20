{-# LANGUAGE LambdaCase #-}

module CLIPG.Drawing (
   horizontal, vertical, up, down, left, right,
   Render(..), (&), curve, heavy, double, overlay
) where

import Data.Bimap (Bimap, (!), (!>))
import qualified Data.Bimap as Bimap
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Applicative ((<|>))
import Data.List ((\\))
import Data.Maybe (fromJust)

moduleName = "CLIPG.Drawing"

class Render a where
   build  :: a -> [String]
   render :: a -> IO ()
   render = putStr . unlines . build


errInvalidChar :: String -> Char -> a
errInvalidChar f c = errorWithoutStackTrace
   (moduleName ++ "." ++ f ++ ": Invalid character: " ++ [c])

errInvalidChars :: String -> Char -> Char -> a
errInvalidChars f x y = errorWithoutStackTrace
   (moduleName ++ "." ++ f ++ ": Invalid character combination: " ++ [x] ++ " and " ++ [y])

   
horizontal = '─'
vertical   = '│'
up = '╵'
down = '╷'
left = '╴'
right = '╶'

dbL = '\10001'
dbU = '\10002'
dbR = '\10003'
dbD = '\10004'


data Direction = L | U | R | D
   deriving (Eq, Ord, Enum, Bounded)

direction :: Char -> Direction
direction c = case lookup c mapping of
   Just dir -> dir
   Nothing  -> errInvalidChar "direction" c
   where
   base = [(left, L), (up, U), (right, R), (down, D)]
   mapping = base >>= \(a,b) -> [(a, b), (heavy a, b), (double a, b)]


(?:) :: Ord a => a -> (a,a) -> Bool
x ?: (a,b) = a <= x && x <= b

(?<) :: (Ord a, Foldable t) => t a -> (a,a) -> Bool
xs ?< (a,b) = all (?: (a,b)) xs


-- curve down left = '╮'
curve :: Char -> Char
curve = \case
   '┌' -> '╭'
   '┐' -> '╮'
   '└' -> '╰'
   '┘' -> '╯'
   chr -> errInvalidChar "curve" chr


(&) :: Char -> Char -> Char
x & y | x == y = x
x & y = if null z
   then errInvalidChars "&" x y
   else fromJust z
   where z = (sequence $ atoms <$> [x,y]) >>= fromAtoms . Set.unions


overlay :: Char -> Char -> Char
overlay ' ' y = y
overlay x y = if null z
   then errInvalidChars "overlay" x y
   else fromJust z
   where
   z = (sequence $ atoms <$> [x,y]) >>= \[xs,ys] -> fromAtoms . Set.union xs $
      Set.filter (flip Set.notMember (Set.map direction xs) . direction) ys


heavy :: Char -> Char
heavy c
   | c == ' ' = c
   | c?:('─','━') || c=='╼' || c=='╾' = '━'
   | c?:('│','┃') || c=='╽' || c=='╿' = '┃'
   | c?:('╌','╍') = '╍'
   | c?:('╎','╏') = '╏'
   | c?:('┄','┅') = '┅'
   | c?:('┆','┇') = '┇'
   | c?:('┈','┉') = '┉'
   | c?:('┊','┋') = '┋'
   | c `elem` "╵╹" = '╹'
   | c `elem` "╷╻" = '╻'
   | c `elem` "╴╸" = '╸'
   | c `elem` "╶╺" = '╺'
   | c?:('┌','┏') || c?:('╒','╔') = '┏'
   | c?:('┐','┓') || c?:('╕','╗') = '┓'
   | c?:('└','┗') || c?:('╘','╚') = '┗'
   | c?:('┘','┛') || c?:('╛','╝') = '┛'
   | c?:('├','┣') || c?:('╞','╠') = '┣'
   | c?:('┤','┫') || c?:('╡','╣') = '┫'
   | c?:('┬','┳') || c?:('╤','╦') = '┳'
   | c?:('┴','┻') || c?:('╧','╩') = '┻'
   | c?:('┼','╋') || c?:('╪','╬') = '╋'
   | otherwise = errInvalidChar "heavy" c

double :: Char -> Char
double c
   | c == ' ' = c
   | c?:('─','━') || c=='╼' || c=='╾' = '═'
   | c?:('│','┃') || c=='╽' || c=='╿' = '║'
   | c `elem` "╵╹" = dbU
   | c `elem` "╷╻" = dbD
   | c `elem` "╴╸" = dbL
   | c `elem` "╶╺" = dbR
   | c?:('┌','┏') || c?:('╒','╔') = '╔'
   | c?:('┐','┓') || c?:('╕','╗') = '╗'
   | c?:('└','┗') || c?:('╘','╚') = '╚'
   | c?:('┘','┛') || c?:('╛','╝') = '╝'
   | c?:('├','┣') || c?:('╞','╠') = '╠'
   | c?:('┤','┫') || c?:('╡','╣') = '╣'
   | c?:('┬','┳') || c?:('╤','╦') = '╦'
   | c?:('┴','┻') || c?:('╧','╩') = '╩'
   | c?:('┼','╋') || c?:('╪','╬') = '╬'
   | otherwise = errInvalidChar "double" c

atoms :: Char -> Maybe (Set Char)
atoms = (`Bimap.lookup` atomMap)

fromAtoms :: Set Char -> Maybe Char
fromAtoms = (`Bimap.lookupR` atomMap)

atomMap :: Bimap Char (Set Char)
atomMap = Bimap.fromList list
   where
   list = [(c, Set.singleton c) | c <- ['╴'..'╻'] ++ [dbL,dbU,dbR,dbD]]
      ++ [(c, atoms' c) | c <- "─━│┃╼╽╾╿" ++ (['┌'..'╬'] \\ "╌╍╎╏")]
   
   atoms' :: Char -> Set Char
   atoms' = Set.fromList . \case
      -- lines
      '─' -> [left, right]
      '━' -> [heavy left, heavy right]
      '│' -> [up, down]
      '┃' -> [heavy up, heavy down]
      '╼' -> [left, heavy right]
      '╽' -> [up, heavy down]
      '╾' -> [heavy left, right]
      '╿' -> [heavy up, down]
      '═' -> [double left, double right]
      '║' -> [double up, double down]
      -- bends
      '┌' -> [right, down]
      '┍' -> [heavy right, down]
      '┎' -> [right, heavy down]
      '┏' -> [heavy right, heavy down]
      '┐' -> [left, down]
      '┑' -> [heavy left, down]
      '┒' -> [left, heavy down]
      '┓' -> [heavy left, heavy down]
      '└' -> [up, right]
      '┕' -> [up, heavy right]
      '┖' -> [heavy up, right]
      '┗' -> [heavy up, heavy right]
      '┘' -> [left, up]
      '┙' -> [heavy left, up]
      '┚' -> [left, heavy up]
      '┛' -> [heavy left, heavy up]
      '╒' -> [double right, down]
      '╓' -> [right, double down]
      '╔' -> [double right, double down]
      '╕' -> [double left, down]
      '╖' -> [left, double down]
      '╗' -> [double left, double down]
      '╘' -> [up, double right]
      '╙' -> [double up, right]
      '╚' -> [double up, double right]
      '╛' -> [double left, up]
      '╜' -> [left, double up]
      '╝' -> [double left, double up]
      -- junctions
      '├' -> [up, down, right]
      '┝' -> [up, down, heavy right]
      '┞' -> [heavy up, down, right]
      '┟' -> [up, heavy down, right]
      '┠' -> [heavy up, heavy down, right]
      '┡' -> [heavy up, down, heavy right]
      '┢' -> [up, heavy right, heavy down]
      '┣' -> [heavy up, heavy right, heavy down]
      '┤' -> [left, up, down]
      '┥' -> [heavy left, up, down]
      '┦' -> [left, heavy up, down]
      '┧' -> [left, up, heavy down]
      '┨' -> [left, heavy up, heavy down]
      '┩' -> [heavy left, heavy up, down]
      '┪' -> [heavy left, up, heavy down]
      '┫' -> [heavy left, heavy up, heavy down]
      '┬' -> [left, right, down]
      '┭' -> [heavy left, right, down]
      '┮' -> [left, heavy right, down]
      '┯' -> [heavy left, heavy right, down]
      '┰' -> [left, right, heavy down]
      '┱' -> [heavy left, right, heavy down]
      '┲' -> [left, heavy right, heavy down]
      '┳' -> [heavy left, heavy right, heavy down]
      '┴' -> [left, up, right]
      '┵' -> [heavy left, up, right]
      '┶' -> [left, up, heavy right]
      '┷' -> [heavy left, up, heavy right]
      '┸' -> [left, heavy up, right]
      '┹' -> [heavy left, heavy up, right]
      '┺' -> [left, heavy up, heavy right]
      '┻' -> [heavy left, heavy up, heavy right]
      '╞' -> [up, double right, down]
      '╟' -> [double up, right, double down]
      '╠' -> [double up, double right, double down]
      '╡' -> [double left, up, down]
      '╢' -> [left, double up, double down]
      '╣' -> [double left, double up, double down]
      '╤' -> [double left, double right, down]
      '╥' -> [left, right, double down]
      '╦' -> [double left, double right, double down]
      '╧' -> [double left, up, double right]
      '╨' -> [left, double up, right]
      '╩' -> [double left, double up, double right]
      -- crosses
      '┼' -> [left, up, right, down]
      '┽' -> [heavy left, up, right, down]
      '┾' -> [left, up, heavy right, down]
      '┿' -> [heavy left, up, heavy right, down]
      '╀' -> [left, heavy up, right, down]
      '╁' -> [left, up, right, heavy down]
      '╂' -> [left, heavy up, right, heavy down]
      '╃' -> [heavy left, heavy up, right, down]
      '╄' -> [left, heavy up, heavy right, down]
      '╅' -> [heavy left, up, right, heavy down]
      '╆' -> [left, up, heavy right, heavy down]
      '╇' -> [heavy left, heavy up, heavy right, down]
      '╈' -> [heavy left, up, heavy right, heavy down]
      '╉' -> [heavy left, heavy up, right, heavy down]
      '╊' -> [left, heavy up, heavy right, heavy down]
      '╋' -> [heavy left, heavy up, heavy right, heavy down]
      '╪' -> [double left, up, double right, down]
      '╫' -> [left, double up, right, double down]
      '╬' -> [double left, double up, double right, double down]
      chr -> errInvalidChar "atoms" chr

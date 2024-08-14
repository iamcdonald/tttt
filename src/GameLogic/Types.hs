module GameLogic.Types (Coord (..)) where

data Coord = Coord {x :: Int, y :: Int} deriving (Eq, Show)

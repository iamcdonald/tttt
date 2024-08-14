module Board (Board, BoardException (..), make, placePiece) where

import Data.Maybe (isJust)
import Types

type Board a = [[Maybe a]]

data BoardException = InvalidBoardDimension | InvalidBoardCoord | CoordIsOccupied deriving (Eq, Show)

make :: (Eq a, Show a) => Int -> Either (Board a) BoardException
make d
  | d > 0 = Left [[Nothing | _ <- [1 .. d]] | _ <- [1 .. d]]
  | otherwise = Right InvalidBoardDimension

placePiece :: (Eq a) => Board a -> a -> Types.Coord -> Either (Board a) BoardException
placePiece board piece coord@Types.Coord {x, y} =
  case (exists, occupied) of
    (False, _) -> Right InvalidBoardCoord
    (_, True) -> Right CoordIsOccupied
    (True, False) -> Left $ replace x (\r -> replace y (\_ -> Just piece) r) board
  where
    exists = coord `existsOn` board
    occupied = coord `isOccupiedOn` board

replace :: Int -> (a -> a) -> [a] -> [a]
replace n f l
  | n >= length l = l
  | otherwise = take n l ++ [f $ l !! n] ++ drop (n + 1) l

existsOn :: Types.Coord -> Board a -> Bool
existsOn Types.Coord {x, y} b
  | x < 0 = False
  | y < 0 = False
  | x >= length b = False
  | y >= length (b !! x) = False
  | otherwise = True

isOccupiedOn :: (Eq a) => Types.Coord -> Board a -> Bool
isOccupiedOn Types.Coord {x, y} b = isJust ((b !! x) !! y)

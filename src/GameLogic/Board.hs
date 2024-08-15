module GameLogic.Board (Board, BoardException (..), Bounds(..), Bound(..), make, placePiece, extractLines, isFull, getBounds, getCoords) where

import Data.List
import Data.Maybe (isJust)
import GameLogic.Types qualified as T
import Control.Exception (Exception)

type Board a = [[Maybe a]]

data BoardException = InvalidBoardDimension | InvalidBoardCoord | CoordIsOccupied deriving (Eq, Show)
instance Exception BoardException

data Bound = Bound {min :: Int, max :: Int}

data Bounds = Bounds
  { x :: Bound,
    y :: Bound
  }

make :: (Eq a, Show a) => Int -> Either (Board a) BoardException
make d
  | d > 0 = Left [[Nothing | _ <- [1 .. d]] | _ <- [1 .. d]]
  | otherwise = Right InvalidBoardDimension

placePiece :: (Eq a) => Board a -> a -> T.Coord -> Either (Board a) BoardException
placePiece board piece coord@(T.Coord x y) =
  case (exists, occupied) of
    (False, _) -> Right InvalidBoardCoord
    (_, True) -> Right CoordIsOccupied
    (True, False) -> Left $ replace y (\r -> replace x (\_ -> Just piece) r) board
  where
    exists = coord `existsOn` board
    occupied = coord `isOccupiedOn` board

replace :: Int -> (a -> a) -> [a] -> [a]
replace n f l
  | n >= length l = l
  | otherwise = take n l ++ [f $ l !! n] ++ drop (n + 1) l

existsOn :: T.Coord -> Board a -> Bool
existsOn (T.Coord x y) b
  | x < 0 = False
  | y < 0 = False
  | y >= length b = False
  | x >= length (b !! y) = False
  | otherwise = True

isOccupiedOn :: (Eq a) => T.Coord -> Board a -> Bool
isOccupiedOn (T.Coord x y) b = isJust ((b !! y) !! x)

extractLines :: Board a -> [[Maybe a]]
extractLines b = rows ++ columns ++ diag1 ++ diag2
  where
    rows = b
    columns = transpose rows
    diag1 = transpose (zipWith drop [0 ..] rows)
    diag2 = transpose (zipWith drop [0 ..] (map reverse rows))

isFull :: (Eq a) => Board a -> Bool
isFull = all (all isJust)

getCoords :: Board a -> (Maybe a -> Bool) -> [T.Coord]
getCoords board predicate =
  [ (T.Coord x y)
    | (y, row) <- zip [0 ..] board,
      (x, entry) <- zip [0 ..] row,
      predicate entry
  ]

getBounds :: Board a -> Bounds
getBounds b =
  case coords of
    [] -> (Bounds (Bound 0 0) (Bound 0 0))
    [(T.Coord x y)] -> (Bounds (Bound x x) (Bound y y))
    ((T.Coord x y):cs) -> foldl' asBounds (Bounds (Bound x x) (Bound y y)) cs
  where
    coords = getCoords b (\_ -> True)
    asBounds (Bounds (Bound minX maxX) (Bound minY maxY)) (T.Coord ax ay) =
      (Bounds
        (Bound (minimum [minX, ax]) (maximum [maxX, ax]))
        (Bound (minimum [minY, ay]) (maximum [maxY, ay]))
      )

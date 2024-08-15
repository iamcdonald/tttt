module UI.State (State (..), Command (..), moveCursor) where

import GameLogic.Board as Board
import GameLogic.Types qualified as T
import Data.List (minimumBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)

data Command = Up | Down | Left | Right deriving (Eq, Show)

newtype State = State
  { cursor :: T.Coord
  }
  deriving (Eq, Show)

distanceBetweenPoints :: T.Coord -> T.Coord -> Int
distanceBetweenPoints (T.Coord c1x c1y) (T.Coord c2x c2y) =
  ((c2x - c1x) ^ (2 :: Int)) + ((c2y - c1y) ^ (2 :: Int))

sortPointDistance :: T.Coord -> T.Coord -> Int
sortPointDistance cRef c = do
  distanceBetweenPoints cRef c

nearestAvailable :: [T.Coord] -> T.Coord -> T.Coord
nearestAvailable [] coord = coord
nearestAvailable available coord = do
  minimumBy (comparing distance) available
    where
      distance = sortPointDistance coord

bounded :: Board a -> T.Coord -> T.Coord
bounded b (T.Coord x y) =
  (T.Coord bx by)
  where
    (Bounds (Bound minX maxX) (Bound minY maxY)) = Board.getBounds b
    bx
      | x < minX = maxX
      | x > maxX = minX
      | otherwise = x
    by
      | y < minY = maxY
      | y > maxY = minY
      | otherwise = y

moveCursor :: Board a -> State -> Command -> State
moveCursor b s cmd
  | cmd == UI.State.Up = asState s c {T.y = y - 1}
  | cmd == UI.State.Down = asState s c {T.y = y + 1}
  | cmd == UI.State.Left = asState s c {T.x = x - 1}
  | otherwise = asState s c {T.x = x + 1}
  where
    c = cursor s
    (x,y) = (T.x c, T.y c)
    asState state uc =
      state {cursor = checkAvailable $ bounded b uc}
      where
        available = filter (/= c) (Board.getCoords b isNothing)
        checkAvailable ac
          | null available = ac
          | ac `elem` available = ac
          | otherwise = nearestAvailable available ac

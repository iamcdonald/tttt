module Game (Game (..), Player (..), GameState (..), Piece (..), Game.make, play) where

import Board
import Data.List
import Types

data Piece = X | O deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

data GameState = Playing | Draw | Win deriving (Eq, Show)

data Game = Game
  { size :: Int,
    board :: Board.Board Piece,
    player :: Player,
    err :: Maybe Board.BoardException,
    state :: GameState,
    winner :: Maybe Player
  }
  deriving (Show)

getPlayerPiece :: Player -> Piece
getPlayerPiece Player1 = X
getPlayerPiece Player2 = O

make :: Int -> Either Game BoardException
make size =
  case board of
    Left b -> Left Game {board = b, player = Player1, err = Nothing, state = Playing, size = size, winner = Nothing}
    Right err -> Right err
  where
    board = Board.make size

play :: Game -> Types.Coord -> Game
play g c =
  case nextBoard of
    Left nb -> do
      let (nextState, winner) = getGameState g {board = nb}
      g {board = nb, player = nextPlayer, state = nextState, winner = winner}
    Right err -> g {err = Just err}
  where
    p = player g
    piece = getPlayerPiece p
    nextPlayer = case p of
      Player1 -> Player2
      Player2 -> Player1
    nextBoard = Board.placePiece (board g) piece c

hasWon :: Int -> [[Maybe Piece]] -> Piece -> Bool
hasWon seqSize ls piece =
  case find (isInfixOf expected) ls of
    Just _ -> True
    Nothing -> False
  where
    expected = [Just piece | _ <- [1 :: Int .. seqSize]]

getGameState :: Game -> (GameState, Maybe Player)
getGameState Game {board, size}
  | player1Won = (Win, Just Player1)
  | player2Won = (Win, Just Player2)
  | draw = (Draw, Nothing)
  | otherwise = (Playing, Nothing)
  where
    ls = Board.extractLines board
    player1Won = hasWon size ls (getPlayerPiece Player1)
    player2Won = hasWon size ls (getPlayerPiece Player2)
    draw = Board.isFull board

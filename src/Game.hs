module Game where

import Board

data Piece = X | O deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

data Game = Game {board :: Board.Board Piece, player :: Player, err :: Maybe Board.BoardException} deriving (Show)

getPlayerPiece :: Player -> Piece
getPlayerPiece Player1 = X
getPlayerPiece Player2 = O

make :: Int -> Either Game BoardException
make size =
  case board of
    Left b -> Left Game {board = b, player = Player1, err = Nothing}
    Right err -> Right err
  where
    board = Board.make size

play :: Game -> Board.Coord -> Game
play g c =
  case nextBoard of
    Left nb -> g {board = nb, player = nextPlayer}
    Right err -> g {err = Just err}
  where
    p = player g
    piece = getPlayerPiece p
    nextPlayer = case p of
      Player1 -> Player2
      Player2 -> Player1
    nextBoard = Board.placePiece (board g) piece c

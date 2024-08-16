module UI.Renderer where

import Data.Maybe (fromJust, isJust)
import GameLogic.Board
import GameLogic.Game qualified as Game
import GameLogic.Types qualified as T
import UI.State

data Position = Top | Middle | Bottom

data Cell a = Cell {active :: Bool, piece :: Maybe a} deriving (Eq, Show)

renderCellContents :: (Show a) => Maybe a -> [Char]
renderCellContents x = if isJust x then (show $ fromJust x) else " "

renderCell :: (Show a) => Cell a -> [Char]
renderCell Cell {active, piece} = if active then "." else renderCellContents piece

getHorizontalDividerChar :: [Char] -> Int -> [Char]
getHorizontalDividerChar char idx =
  case remain of
    0 -> char
    _ -> "\x2500"
  where
    remain = idx `rem` 4

renderHorizontalDivider :: Position -> Int -> [Char]
renderHorizontalDivider Top len =
  "\x250c" ++ concatMap gc [1 .. len - 2] ++ "\x2510"
  where
    gc = getHorizontalDividerChar "\x252c"
renderHorizontalDivider Middle len =
  "\x251c" ++ concatMap gc [1 .. len - 2] ++ "\x2524"
  where
    gc = getHorizontalDividerChar "\x253c"
renderHorizontalDivider Bottom len =
  "\x2514" ++ concatMap gc [1 .. len - 2] ++ "\x2518"
  where
    gc = getHorizontalDividerChar "\x2534"

wrapRow :: Position -> [Char] -> [Char]
wrapRow pos row =
  topDivider ++ "\n" ++ rowWithDividers ++ "\n" ++ bottomDivider
  where
    rowWithDividers = concatMap (\c -> "\x2502" ++ [' ', c, ' ']) row ++ "\x2502"
    dividerLength = length rowWithDividers
    topDivider = case pos of
      Top -> renderHorizontalDivider Top dividerLength
      _ -> renderHorizontalDivider Middle dividerLength
    bottomDivider = case pos of
      Bottom -> renderHorizontalDivider Bottom dividerLength ++ "\n"
      _ -> ""

renderRow :: (Show a) => (Position, [Cell a]) -> [Char]
renderRow (pos, row) =
  wrapRow pos $ concatMap renderCell row

getRowPosition :: Int -> Int -> Position
getRowPosition idx len
  | idx == 0 = Top
  | idx == len - 1 = Bottom
  | otherwise = Middle

withRowPosition :: (Show a) => [[Cell a]] -> [(Position, [Cell a])]
withRowPosition grid =
  zipWith (\idx r -> (getRowPosition idx gl, r)) [0 ..] grid
  where
    gl = length grid

renderBoard :: (Show a) => (UI.State.State, Board a) -> Bool -> [Char]
renderBoard (UI.State.State {cursor}, board) showCursor = do
  concatMap renderRow (withRowPosition grid)
  where
    isActive = (== cursor)
    grid =
      [ [ (Cell {active = showCursor && isActive (T.Coord x y), piece = p})
          | (x, p) <- zip [0 :: Int ..] row
        ]
        | (y, row) <- zip [0 :: Int ..] board
      ]

renderPlayer :: Game.Player -> Bool -> [Char]
renderPlayer p withPiece
  | withPiece = player ++ " : " ++ piece
  | otherwise = player
  where
    piece = show $ Game.getPlayerPiece p
    player = case p of
      Game.Player1 -> "Player 1"
      Game.Player2 -> "Player 2"

renderGame :: (State, Game.Game) -> [Char]
renderGame (state, game)
  | gameState == Game.Playing = case err of
      Just _ -> rb True ++ p ++ "\n" ++ e
      Nothing -> rb True ++ p
  | gameState == Game.Win = rb False ++ "Game Over\nWinner -> " ++ w
  | otherwise = rb False ++ "Game Over\nDraw!"
  where
    Game.Game {Game.board = board, Game.player = player, Game.state = gameState, Game.winner = winner, Game.err = err} = game
    rb = renderBoard (state, board)
    p = renderPlayer player True
    e
      | err == Just CoordIsOccupied = "board position is occupied!"
      | err == Just InvalidBoardCoord = "board position does not exist!"
      | otherwise = ""
    w = case winner of
      Nothing -> "Unknown Player"
      Just wi -> renderPlayer wi False

module UI.StateTest (suite) where

import GameLogic.Types (Coord (..))
import Test.Tasty
import Test.Tasty.HUnit
import UI.State as State

data Piece = L | P deriving (Eq, Show)

suite :: TestTree
suite =
  testGroup
    "UI.State"
    [ moveCursorSuite
    ]

moveCursorSuite :: TestTree
moveCursorSuite =
  testGroup
    "moveCursor"
    [ testCase "up \x2191" move_cursor_up,
      testCase "up \x2191 - out of bounds" move_cursor_up_out_of_bounds,
      testCase "up \x2191 - when occupied choose nearest" move_cursor_up_choose_nearest_available_coord_if_occupied,
      testCase "down \x2193" move_cursor_down,
      testCase "down \x2193 - out of bounds" move_cursor_down_out_of_bounds,
      testCase "down \x2193 - when occupied choose nearest" move_cursor_down_choose_nearest_available_coord_if_occupied,
      testCase "left \x2190" move_cursor_left,
      testCase "left \x2190 - out of bounds" move_cursor_left_out_of_bounds,
      testCase "left \x2190 - when occupied choose nearest" move_cursor_left_choose_nearest_available_coord_if_occupied,
      testCase "right \x2192" move_cursor_right,
      testCase "right \x2192 - out of bounds" move_cursor_right_out_of_bounds,
      testCase "right \x2192 - when occupied choose nearest" move_cursor_right_choose_nearest_available_coord_if_occupied
    ]
  where
    emptyBoard = [[Nothing | _ <- [1 :: Int .. 3]] | _ <- [1 :: Int .. 3]]
    move_cursor_up :: Assertion
    move_cursor_up =
      assertEqual "moves up" (Coord 1 0) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 1)) State.CommandUp

    move_cursor_down :: Assertion
    move_cursor_down =
      assertEqual "moves down" (Coord 1 2) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 1)) State.CommandDown

    move_cursor_left :: Assertion
    move_cursor_left =
      assertEqual "moves left" (Coord 0 1) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 1)) State.CommandLeft

    move_cursor_right :: Assertion
    move_cursor_right =
      assertEqual "moves right" (Coord 2 1) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 1)) State.CommandRight

    move_cursor_up_out_of_bounds :: Assertion
    move_cursor_up_out_of_bounds = assertEqual "loops" (Coord 1 2) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 0)) State.CommandUp

    move_cursor_down_out_of_bounds :: Assertion
    move_cursor_down_out_of_bounds = assertEqual "loops" (Coord 1 0) $ cursor $ State.moveCursor emptyBoard (State (Coord 1 2)) State.CommandDown

    move_cursor_left_out_of_bounds :: Assertion
    move_cursor_left_out_of_bounds = assertEqual "loops" (Coord 2 1) $ cursor $ State.moveCursor emptyBoard (State (Coord 0 1)) State.CommandLeft

    move_cursor_right_out_of_bounds :: Assertion
    move_cursor_right_out_of_bounds = assertEqual "loops" (Coord 0 1) $ cursor $ State.moveCursor emptyBoard (State (Coord 2 1)) State.CommandRight

    board =
      [ [Nothing, Just L, Nothing],
        [Just P, Nothing, Just P],
        [Nothing, Just L, Nothing]
      ]

    move_cursor_up_choose_nearest_available_coord_if_occupied :: Assertion
    move_cursor_up_choose_nearest_available_coord_if_occupied =
      assertEqual "loops" (Coord 0 0) $ cursor $ State.moveCursor board (State (Coord 1 1)) State.CommandUp

    move_cursor_down_choose_nearest_available_coord_if_occupied :: Assertion
    move_cursor_down_choose_nearest_available_coord_if_occupied =
      assertEqual "loops" (Coord 0 2) $ cursor $ State.moveCursor board (State (Coord 1 1)) State.CommandDown

    move_cursor_left_choose_nearest_available_coord_if_occupied :: Assertion
    move_cursor_left_choose_nearest_available_coord_if_occupied =
      assertEqual "loops" (Coord 0 0) $ cursor $ State.moveCursor board (State (Coord 1 1)) State.CommandLeft

    move_cursor_right_choose_nearest_available_coord_if_occupied :: Assertion
    move_cursor_right_choose_nearest_available_coord_if_occupied =
      assertEqual "loops" (Coord 2 0) $ cursor $ State.moveCursor board (State (Coord 1 1)) State.CommandRight

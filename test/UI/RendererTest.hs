module UI.RendererTest (suite) where

import Data.List (intercalate)
import GameLogic.Types qualified as T
import GameLogic.Game
import GameLogic.Board
import Test.Tasty
import Test.Tasty.HUnit
import UI.Renderer as Renderer
import UI.State

data Piece = L | P deriving (Eq, Show)

suite :: TestTree
suite =
  testGroup
    "UI.Renderer"
    [ renderBoardSuite,
      renderGameSuite
    ]

renderBoardSuite :: TestTree
renderBoardSuite =
  testGroup
    "renderBoard"
    [ testCase "render board" render_board,
      testCase "render board with cursor" render_board_with_cursor
    ]
  where
    render_board :: Assertion
    render_board =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderBoard (state, board) False)
      where
        state = UI.State.State {cursor = (T.Coord 2 0)}
        board =
          [ [Just L, Just P, Nothing],
            [Just P, Nothing, Just P],
            [Just L, Just L, Just P]
          ]
        expected =
          [ "┌───┬───┬───┐",
            "│ L │ P │   │",
            "├───┼───┼───┤",
            "│ P │   │ P │",
            "├───┼───┼───┤",
            "│ L │ L │ P │",
            "└───┴───┴───┘",
            ""
          ]

    render_board_with_cursor :: Assertion
    render_board_with_cursor =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderBoard (state, board) True)
      where
        state = UI.State.State {cursor = (T.Coord 2 0)}
        board =
          [ [Just L, Just P, Nothing],
            [Just P, Nothing, Just P],
            [Just L, Just L, Just P]
          ]
        expected =
          [ "┌───┬───┬───┐",
            "│ L │ P │ . │",
            "├───┼───┼───┤",
            "│ P │   │ P │",
            "├───┼───┼───┤",
            "│ L │ L │ P │",
            "└───┴───┴───┘",
            ""
          ]


renderGameSuite :: TestTree
renderGameSuite =
  testGroup
    "renderGame"
    [ testCase "render game : playing" render_game_playing,
      testCase "render game : win" render_game_win,
      testCase "render game : draw" render_game_draw,
      testCase "render game : playing -> error" render_game_playing_error
    ]
  where
    render_game_playing :: Assertion
    render_game_playing =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderGame (state, game))
      where
        state = UI.State.State {cursor = (T.Coord 2 0)}
        board =
          [ [Just X, Just O, Nothing],
            [Just O, Just X, Just O],
            [Just X, Just X, Just O]
          ]
        game = Game{ size = 3, board = board, player = Player1, err = Nothing, winner = Nothing, state = Playing}
        expected =
          [ "┌───┬───┬───┐",
            "│ X │ O │ . │",
            "├───┼───┼───┤",
            "│ O │ X │ O │",
            "├───┼───┼───┤",
            "│ X │ X │ O │",
            "└───┴───┴───┘",
            "Player 1 : X"
          ]

    render_game_win :: Assertion
    render_game_win =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderGame (state, game))
      where
        state = UI.State.State {cursor = (T.Coord 0 1)}
        board =
          [ [Just O, Just X, Nothing],
            [Nothing, Just O, Just X],
            [Just X, Just O, Just O]
          ]
        game = Game{ size = 3, board = board, player = Player2, err = Nothing, winner = Just Player2, state = Win}
        expected =
          [ "┌───┬───┬───┐",
            "│ O │ X │   │",
            "├───┼───┼───┤",
            "│   │ O │ X │",
            "├───┼───┼───┤",
            "│ X │ O │ O │",
            "└───┴───┴───┘",
            "Game Over",
            "Winner -> Player 2"
          ]

    render_game_draw :: Assertion
    render_game_draw =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderGame (state, game))
      where
        state = UI.State.State {cursor = (T.Coord 0 1)}
        board =
          [ [Just O, Just X, Just X],
            [Just O, Just X, Just X],
            [Just X, Just O, Just O]
          ]
        game = Game{ size = 3, board = board, player = Player2, err = Nothing, winner = Nothing, state = Draw}
        expected =
          [ "┌───┬───┬───┐",
            "│ O │ X │ X │",
            "├───┼───┼───┤",
            "│ O │ X │ X │",
            "├───┼───┼───┤",
            "│ X │ O │ O │",
            "└───┴───┴───┘",
            "Game Over",
            "Draw!"
          ]

    render_game_playing_error :: Assertion
    render_game_playing_error =
      assertEqual "render board" (intercalate "\n" expected) (Renderer.renderGame (state, game))
      where
        state = UI.State.State {cursor = (T.Coord 1 1)}
        board =
          [ [Nothing, Just X, Just X],
            [Just O, Nothing, Just X],
            [Just X, Just O, Just O]
          ]
        game = Game{ size = 3, board = board, player = Player2, err = Just CoordIsOccupied, winner = Nothing, state = Playing}
        expected =
          [ "┌───┬───┬───┐",
            "│   │ X │ X │",
            "├───┼───┼───┤",
            "│ O │ . │ X │",
            "├───┼───┼───┤",
            "│ X │ O │ O │",
            "└───┴───┴───┘",
            "Player 2 : O",
            "board position is occupied!"
          ]

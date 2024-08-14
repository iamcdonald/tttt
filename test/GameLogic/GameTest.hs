module GameLogic.GameTest (suite) where

import Control.Exception (Exception, throw)
import Data.Maybe
import GameLogic.Board as Board
import GameLogic.Game as Game
import GameLogic.Types (Coord (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data ShouldReturnException = ShouldReturnBoard | ShouldReturnGame | ShouldReturnException deriving (Eq, Show)

instance Exception ShouldReturnException

suite :: TestTree
suite =
  testGroup
    "GameLogic.Game"
    [ makeSuite,
      playSuite
    ]

makeSuite :: TestTree
makeSuite =
  testGroup
    "make"
    [ testProperty "valid dimension : returns initialised game" valid,
      testProperty "invalid dimension : returns BoardException" invalid
    ]
  where
    valid :: Int -> Property
    valid d =
      (d >= 1) ==> check $ Game.make d
      where
        checkBoard Game {board = b} = length b == d && all (\r -> (length r) == d) b
        checkPlayer Game {player = p} = p == Game.Player1
        checkNoError Game {err} = isNothing err
        checkPlayingState Game {state = s} = s == Game.Playing
        checkSize Game {size = s} = s == d
        checkNoWinner Game {winner = w} = isNothing w
        check (Left g) = checkBoard g && checkPlayer g && checkNoError g && checkPlayingState g && checkSize g && checkNoWinner g
        check (Right _) = throw ShouldReturnGame

    invalid :: Int -> Property
    invalid d =
      (d <= 0) ==> case g of
        Left _ -> throw ShouldReturnException
        Right err -> err == InvalidBoardDimension
      where
        g = Game.make d

playSuite :: TestTree
playSuite =
  testGroup
    "play"
    [ testProperty "valid coord : initialises game" valid_coord,
      testCase "game state: win - horizontal -" game_state_win_horizontal,
      testCase "game state: win - vertical |" game_state_win_vertical,
      testCase "game state: win - diagonal \\" game_state_win_diagonal_1,
      testCase "game state: win - diagonal /" game_state_win_diagonal_2,
      testCase "game state: playing" game_state_playing,
      testCase "game state: draw" game_state_draw,
      testProperty "invalid coord : out of bounds records err on game" invalid_coord_out_of_bounds,
      testProperty "invalid coord : occupied records err on game" invalid_coord_occupied
    ]
  where
    valid_coord :: Property
    valid_coord =
      forAll validArgs $
        \(c, g) -> check c $ Game.play g c
      where
        validArgs = do
          d <- chooseInt (1, 10)
          x <- chooseInt (0, d - 1)
          y <- chooseInt (0, d - 1)
          let g = case (Game.make d) of
                Left game -> game
                Right _ -> throw ShouldReturnGame
          return ((Coord x y), g)
        checkPiecePlaced Coord {x, y} Game {board = b} = (b !! y !! x) == Just X
        checkPlayerSwapped Game {player = p} = p == Player2
        checkNoError Game {err = e} = isNothing e
        check c g = checkPiecePlaced c g && checkPlayerSwapped g && checkNoError g

    game_state_win_horizontal :: Assertion
    game_state_win_horizontal = do
      assertEqual "should be winning" Game.Win state
      assertEqual "should have winner" (Just Game.Player1) winner
      where
        populatedBoard =
          [ [Just X, Nothing, Just X],
            [Nothing, Nothing, Nothing],
            [Nothing, Nothing, Nothing]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player1,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 1 0)

    game_state_win_vertical :: Assertion
    game_state_win_vertical = do
      assertEqual "should be winning" Game.Win state
      assertEqual "should have winner" (Just Game.Player2) winner
      where
        populatedBoard =
          [ [Nothing, Just O, Nothing],
            [Nothing, Just O, Nothing],
            [Nothing, Nothing, Nothing]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player2,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 1 2)

    game_state_win_diagonal_1 :: Assertion
    game_state_win_diagonal_1 = do
      assertEqual "should be winning" Game.Win state
      assertEqual "should have winner" (Just Game.Player2) winner
      where
        populatedBoard =
          [ [Just O, Nothing, Nothing],
            [Nothing, Just O, Nothing],
            [Nothing, Nothing, Nothing]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player2,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 2 2)

    game_state_win_diagonal_2 :: Assertion
    game_state_win_diagonal_2 = do
      assertEqual "should be winning" Game.Win state
      assertEqual "should have winner" (Just Game.Player1) winner
      where
        populatedBoard =
          [ [Nothing, Nothing, Just X],
            [Nothing, Nothing, Nothing],
            [Just X, Nothing, Nothing]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player1,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 1 1)

    game_state_playing :: Assertion
    game_state_playing = do
      assertEqual "should be winning" Game.Playing state
      assertEqual "should have winner" Nothing winner
      where
        populatedBoard =
          [ [Just O, Nothing, Just X],
            [Nothing, Nothing, Nothing],
            [Just X, Nothing, Just O]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player2,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 1 2)

    game_state_draw :: Assertion
    game_state_draw = do
      assertEqual "should be winning" Game.Draw state
      assertEqual "should have winner" Nothing winner
      where
        populatedBoard =
          [ [Just O, Just X, Just X],
            [Nothing, Just O, Just O],
            [Just X, Just O, Just X]
          ]
        Game {state, winner} =
          Game.play
            ( Game
                { size = 3,
                  board = populatedBoard,
                  player = Game.Player1,
                  err = Nothing,
                  state = Game.Playing,
                  winner = Nothing
                }
            )
            (Coord 0 1)

    invalid_coord_out_of_bounds :: Property
    invalid_coord_out_of_bounds =
      forAll outOfBoundArgs $
        \(c, g) -> checkError $ Game.play g c
      where
        outOfBoundArgs = do
          d <- chooseInt (1, 10)
          x <- chooseInt (11, 20)
          y <- chooseInt (0, d)
          let game = case (Game.make d) of
                Right _ -> throw ShouldReturnGame
                Left b -> b
          return ((Coord x y), game)
        checkError Game {err} = err == Just Board.InvalidBoardCoord

    invalid_coord_occupied :: Property
    invalid_coord_occupied =
      forAll occupiedArgs $
        \(c, g) -> checkError $ Game.play g c
      where
        occupiedArgs = do
          d <- chooseInt (1, 10)
          x <- chooseInt (0, d - 1)
          y <- chooseInt (0, d - 1)
          let c = (Coord x y)
          let game = case (Game.make d) of
                Right _ -> throw ShouldReturnGame
                Left b -> b
          let played = Game.play game c
          return (c, played)
        checkError Game {err} = err == Just Board.CoordIsOccupied

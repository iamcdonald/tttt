module GameTest (suite) where

import Board (BoardException (..), Coord (..))
import Control.Exception (Exception, throw)
import Data.Maybe
import Game
import Test.Tasty
import Test.Tasty.QuickCheck

data ShouldReturnException = ShouldReturnGame | ShouldReturnException deriving (Eq, Show)

instance Exception ShouldReturnException

suite :: TestTree
suite =
  testGroup
    "Game"
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
        check (Left g) = checkBoard g && checkPlayer g && checkNoError g
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
    "Make"
    [ testProperty "valid coord : places piece and change to next player" valid_coord,
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
          return ((Board.Coord x y), g)
        checkPiecePlaced Board.Coord {x, y} Game {board = b} = (b !! x !! y) == Just X
        checkPlayerSwapped Game {player = p} = p == Player2
        checkNoError Game {err = e} = isNothing e
        check c g = checkPiecePlaced c g && checkPlayerSwapped g && checkNoError g

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
          return ((Board.Coord x y), game)
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
          let c = (Board.Coord x y)
          let game = case (Game.make d) of
                Right _ -> throw ShouldReturnGame
                Left b -> b
          let played = Game.play game c
          return (c, played)
        checkError Game {err} = err == Just Board.CoordIsOccupied

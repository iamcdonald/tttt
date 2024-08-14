module GameLogic.BoardTest (suite) where

import Control.Exception (Exception, throw)
import Data.Maybe
import GameLogic.Board as Board
import GameLogic.Types (Coord (..))
import Test.Tasty
import Test.Tasty.QuickCheck

data Piece = P | I | E | C deriving (Eq, Show, Enum)

data ShouldReturnException = ShouldReturnBoard | ShouldReturnBoardException deriving (Eq, Show)

instance Exception ShouldReturnException

suite :: TestTree
suite =
  testGroup
    "GameLogic.Board"
    [ makeSuite,
      placePieceSuite
    ]

makeSuite :: TestTree
makeSuite =
  testGroup
    "make"
    [ testProperty "valid dimension : returns 2d board" valid_dimension,
      testProperty "valid dimension : sets entire board to Nothing" valid_content,
      testProperty "invalid dimension : returns InvalidBoardDimension" invalid_dimension
    ]
  where
    valid_dimension :: Int -> Property
    valid_dimension d =
      (d >= 1) ==> check $ Board.make @Piece d
      where
        check (Left b) = length b == d && all (\r -> (length r) == d) b
        check (Right _) = throw ShouldReturnBoard

    valid_content :: Int -> Property
    valid_content d =
      (d >= 1) ==> check $ Board.make @Piece d
      where
        check (Left b) = all (all isNothing) b
        check (Right _) = throw ShouldReturnBoard

    invalid_dimension :: Int -> Property
    invalid_dimension d =
      (d <= 0)
        ==> case b of
          Left _ -> throw ShouldReturnBoardException
          Right err -> err == InvalidBoardDimension
      where
        b = Board.make @Piece d

placePieceSuite :: TestTree
placePieceSuite =
  testGroup
    "placePiece"
    [ testProperty "valid coord : places piece at correct coord" valid_adds_at_correct_coord,
      testProperty "valid coord : places piece once" valid_adds_once,
      testProperty "valid coord : does not alter board dimensions" valid_does_not_alter_board_dimensions,
      testProperty "invalid coord : coord is out of bounds" invalid_coord_out_of_bounds,
      testProperty "invalid coord : coord is already occupied" invalid_coord_occupied
    ]
  where
    placePieceValidArgs :: Gen (Piece, Coord, Board Piece)
    placePieceValidArgs = do
      d <- chooseInt (1, 10)
      x <- chooseInt (0, d - 1)
      y <- chooseInt (0, d - 1)
      p <- elements [P, I, E, C]
      let board = case (Board.make @Piece d) of
            Right _ -> throw ShouldReturnBoard
            Left b -> b
      return (p, (Coord x y), board)

    valid_adds_at_correct_coord :: Property
    valid_adds_at_correct_coord =
      forAll placePieceValidArgs $
        \(p, c, b) -> case (Board.placePiece b p c) of
          Left board -> (getPiece c board) == Just p
          Right _ -> throw ShouldReturnBoard
      where
        getPiece Coord {x, y} b = (b !! x !! y)

    valid_adds_once :: Property
    valid_adds_once =
      forAll placePieceValidArgs $
        \(p, c, b) -> (countPieces $ Board.placePiece b p c) == 1
      where
        countPieces (Right _) = throw ShouldReturnBoard
        countPieces (Left b) = length $ filter isJust $ concat b

    valid_does_not_alter_board_dimensions :: Property
    valid_does_not_alter_board_dimensions =
      forAll placePieceValidArgs $
        \(p, c, b) -> case (Board.placePiece b p c) of
          Left nb -> (countSpaces nb) == (countSpaces b)
          Right _ -> throw ShouldReturnBoard
      where
        countSpaces b = length $ concat b

    invalid_coord_out_of_bounds :: Property
    invalid_coord_out_of_bounds =
      forAll occupiedArgs $
        \(p, c, b) -> case (Board.placePiece b p c) of
          Left _ -> throw ShouldReturnBoardException
          Right err -> err == InvalidBoardCoord
      where
        occupiedArgs = do
          d <- chooseInt (1, 10)
          x <- chooseInt (11, 20)
          y <- chooseInt (0, d)
          p <- elements [P, I, E, C]
          let board = case (Board.make @Piece d) of
                Right _ -> throw ShouldReturnBoard
                Left b -> b
          return (p, (Coord x y), board)

    invalid_coord_occupied :: Property
    invalid_coord_occupied =
      forAll outOfBoundArgs $
        \(p, c, b) -> case (Board.placePiece b p c) of
          Left _ -> throw ShouldReturnBoardException
          Right err -> err == CoordIsOccupied
      where
        outOfBoundArgs = do
          d <- chooseInt (1, 10)
          x <- chooseInt (0, d - 1)
          y <- chooseInt (0, d - 1)
          p <- elements [P, I, E, C]
          let c = (Coord x y)
          let initial = case (Board.make @Piece d) of
                Right _ -> throw ShouldReturnBoard
                Left b -> b
          let played = case (Board.placePiece initial p c) of
                Right _ -> throw ShouldReturnBoard
                Left b -> b
          return (p, c, played)

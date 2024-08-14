module Main where

import GameLogic.BoardTest
import GameLogic.GameTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import UI.RendererTest
import UI.StateTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ GameLogic.BoardTest.suite,
      GameLogic.GameTest.suite,
      UI.StateTest.suite,
      UI.RendererTest.suite
    ]

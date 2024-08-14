module Main where

import BoardTest
import GameTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ BoardTest.suite,
      GameTest.suite
    ]

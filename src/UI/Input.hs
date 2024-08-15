module UI.Input where

import Data.List (isInfixOf)
import System.IO
import UI.State qualified as State

data Command = Enter

command :: [Char] -> Maybe (Either State.Command Command)
command cs
  | "\ESC[A" `isInfixOf` cs = Just $ Left State.CommandUp
  | "\ESC[B" `isInfixOf` cs = Just $ Left State.CommandDown
  | "\ESC[C" `isInfixOf` cs = Just $ Left State.CommandRight
  | "\ESC[D" `isInfixOf` cs = Just $ Left State.CommandLeft
  | "\n" `isInfixOf` cs = Just $ Right Enter
  | otherwise = Nothing

getCommand :: IO (Either State.Command Command)
getCommand = do
  cursor []
  where
    cursor csi = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      char <- getChar
      let chars = csi ++ [char]
      let mcmd = command chars
      case mcmd of
        Nothing -> cursor chars
        Just cmdv -> return cmdv

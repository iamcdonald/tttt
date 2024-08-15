module Main where

import Control.Exception (throw)
import GameLogic.Game qualified as Game
import GameLogic.Types (Coord (..))
import System.IO
import UI.Input
import UI.Renderer
import UI.State qualified

renderToScreen :: (UI.State.State, Game.Game) -> IO ()
renderToScreen ctx = do
  putStr "\ESC[f\ESC[0J"
  putStr $ UI.Renderer.renderGame ctx
  hFlush stdout

makeMove :: (UI.State.State, Game.Game) -> IO (UI.State.State, Game.Game)
makeMove (state@UI.State.State {UI.State.cursor = cursor}, game@Game.Game {Game.state = Game.Playing}) = do
  renderToScreen (state, game)
  cmd <- UI.Input.getCommand
  case cmd of
    Right _ -> do
      let g = Game.play game cursor
      return (UI.State.moveCursor (Game.board g) state UI.State.CommandRight, g)
    Left scmd -> do
      makeMove (UI.State.moveCursor (Game.board game) state scmd, game)
makeMove x = do return x

loop :: (UI.State.State, Game.Game) -> IO (UI.State.State, Game.Game)
loop (state, game@Game.Game {Game.state = Game.Playing}) = do
  ch <- makeMove (state, game)
  loop ch
loop (state, game) = do
  renderToScreen (state, game)
  return (state, game)

playGame :: IO ()
playGame = do
  _ <- loop ((UI.State.State {UI.State.cursor = (Coord 0 0)}), game)
  putStr "\n"
  where
    game = case (Game.make 3) of
      Right e -> throw e
      Left g -> g

main :: IO ()
main = do
  playGame

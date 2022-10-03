module Main where

import Model.TestModel
import Controller.TestController
import View.TestView

import Model.Characters as C
import Model.Items (Position)
import Model.Level hiding (player)
import Model.Game
import Controller.Engine
import View.Terminal

import Control.Concurrent

showCoords :: GameState -> IO ()
showCoords gs = do
  print $ C.pPosition (player gs)

gameLoop :: GameState -> IO GameState
gameLoop gs = do
  putStrLn "1"
  printGame gs
  showCoords gs

  -- | Manual game tick steping
  -- putStrLn "Press enter to continue"
  -- getLine

  -- | Automatic game tick stepping	 timings
  -- | 1sec = 1000000(1 t/s); 
  -- | 0.5sec(2 t/s) = 500000; 
  -- | 0.1sec (10 t/s) = 100000;
  -- | 0.01sec(100 t/s) = 10000; 
  -- | 0.001sec(1000 t/s) = 1000;
  threadDelay 100000 
  gameLoop $ step 2000 gs

main :: IO ()
main = do
  let game = newGame defaultLevel C.defaultPlayer
  let gg = movePlayer C.Right game
  gameLoop gg
  putStrLn "Complete!"

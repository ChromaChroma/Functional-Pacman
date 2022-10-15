module Main where

import View.Gui ( startRender )
import View.Animation

main :: IO ()
main = do
  txtures <- loadTextures
  startRender txtures
  putStrLn "Game Complete!"

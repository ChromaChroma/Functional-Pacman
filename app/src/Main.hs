module Main where

import Model.TestModel
import Controller.TestController
import View.TestView

import Model.Characters
import Model.Level
import Model.Game (newGame)
import View.Terminal

main :: IO ()
main = do
  printGame (newGame defaultLevel defaultPlayer)
  -- showText model
  --   where
  --     model = MkTestModel "name"

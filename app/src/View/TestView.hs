module View.TestView (showText) where

import Config.TestConfig
import Graphics.Gloss
import Model.TestModel

background :: Color
background = white

getName :: TestModel -> Picture
getName (MkTestModel name) = text name

showText :: TestModel -> IO ()
showText m = do
  startRender
  -- display screen background (getName m)
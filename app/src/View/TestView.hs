module View.TestView (showText) where

import Config.TestConfig
import Graphics.Gloss
import Model.TestModel

background :: Color
background = white

-- drawing :: Picture
-- drawing = circle 80

-- showCircle :: IO ()
-- showCircle = do
--   display window background drawing

getName :: TestModel -> Picture
getName (MkTestModel name) = text name

showText :: TestModel -> IO ()
showText m = do
  display window background (getName m)
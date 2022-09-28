module Config.TestConfig where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game


type Model = (Float, Float)

screen :: Display
screen = InWindow "Nice Window" windowSize windowOffsetPosition
-- screen = FullScreen

windowSize :: (Int, Int)
windowSize = (1000, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

fps :: Int
fps = 60

-- main :: IO ()
-- main = simulate
--   screen
--   white
--   simulationRate
--   initialModel
--   drawingFunc
--   updateFunc
--   where
--     simulationRate :: Int
--     simulationRate = 60

--     initialModel :: Model
--     initialModel = (0,0)

--     drawingFunc :: Model -> Picture
--     drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]

--     updateFunc :: ViewPort -> Float -> Model -> Model
--     updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))

startRender :: IO ()
startRender = play
  screen
  white
  fps
  initialModel
  drawingFunc
  inputHandler
  updateFunc
  where
    initialModel :: Model
    initialModel = (0,0)

    drawingFunc :: Model -> Picture
    drawingFunc (x, y) = translate x y (Circle 20)

    inputHandler :: Event -> Model -> Model
    inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
    inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
    inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
    inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
    inputHandler _ w = w

    updateFunc :: Float -> Model -> Model
    updateFunc _ (x, y) = (towardCenter x, towardCenter y)
      where
        towardCenter :: Float -> Float
        towardCenter c
          | abs c < 0.25 = 0
          | c > 0 = c - 0.25
          | otherwise = c + 0.25
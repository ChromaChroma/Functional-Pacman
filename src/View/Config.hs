module View.Config where

import Graphics.Gloss

screen :: Display
screen = InWindow "Pac Man" windowSize windowOffsetPosition
-- screen = FullScreen

windowSize :: (Int, Int)
windowSize = (1300, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

framesPerSecond :: Int
framesPerSecond = 60

tileSize :: Float
tileSize = 24

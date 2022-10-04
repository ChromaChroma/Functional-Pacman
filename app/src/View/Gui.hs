module View.Gui where

import Model.Game
import Model.Characters
import Model.Items
import Model.Score
import Model.Level
import Controller.Engine


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

screen :: Display
screen = InWindow "Nice Window" windowSize windowOffsetPosition
-- screen = FullScreen

windowSize :: (Int, Int)
windowSize = (1000, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

fps :: Int
fps = 60

tileSize :: Float
tileSize = 30

startRender :: IO ()
startRender =
  play
    screen
    white
    fps
    initialModel
    drawingFunc
    inputHandler
    updateFunc
  where
    initialModel :: GameState
    initialModel = defaultGame

    drawingFunc :: GameState -> Picture
    drawingFunc gs = pictures $ [
        text (show (status gs)),
        renderPlayer (player gs)
        ]
    -- drawingFunc (x, y) = translate x y (Circle 20)

    inputHandler :: Event -> GameState -> GameState
    -- inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
    -- inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
    -- inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
    -- inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
    inputHandler _ w = w

    updateFunc :: Float -> GameState -> GameState
    updateFunc _ gs = gs

-- | Returns Pictures, consisting of all tile Pictures
renderLevel :: Level -> Picture
renderLevel = undefined

renderTile :: Tile -> Picture
renderTile = undefined


renderPlayer :: Player -> Picture
renderPlayer p = translate x' y' (circle tileSize)
    where 
        (x, y) = pPosition p
        x' =  (x * tileSize)
        y' =  (x * tileSize)

-- | Returns Pictures (Picture consisting of multiple pictures
renderGhosts :: [Ghost] -> Picture
renderGhosts = undefined

-- | Returns Pictures (Picture consisting of multiple pictures)
renderItems :: [PointItem] -> Picture
renderItems = undefined

renderLives :: Lives -> [Picture]
renderLives = undefined

renderScore :: Score -> Picture
renderScore = undefined
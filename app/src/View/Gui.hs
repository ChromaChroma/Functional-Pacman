module View.Gui where

import Model.Game
import Model.Characters
import Model.Items
import Model.Score
import Model.Movement
import Model.Level
import Controller.Engine


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Data.Data (ConstrRep(FloatConstr))
import Data.Maybe
import Data.List
import Data.List.Index

screen :: Display
screen = InWindow "Pac Man" windowSize windowOffsetPosition
-- screen = FullScreen

windowSize :: (Int, Int)
windowSize = (1000, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

fps :: Int
fps = 10

tileSize :: Float
tileSize = 32

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

    -- | Render game state, aligned from bottom left corner
    drawingFunc :: GameState -> Picture
    drawingFunc gs = fromBottomLeft $ pictures [
        -- smallText . show . status $ gs,
        -- smallText . show . elapsedTime $ gs,
        -- smallText . show . score $ gs,
        -- smallText . show . lives $ gs,
        -- smallText . show . level $ gs,
        smallText . show . pPosition . player $ gs,

        renderLevel defaultLevel,
        -- PointItems

        renderPlayer gs
        -- Render Movable?? ghost and player get rendered same way.
        ]
        where
          fromBottomLeft = translate x' y'
          
          (x, y) = windowSize
          x' = - fromIntegral(x `div` 2) + tileSize / 2
          y' = - fromIntegral (y `div` 2) + tileSize / 2

          smallText = scale 0.1 0.1 . text 

    inputHandler :: Event -> GameState -> GameState
    -- inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
    -- inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
    -- inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
    -- inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
    inputHandler _ w = w

    updateFunc :: Float -> GameState -> GameState
    updateFunc _ gs = step 50 gs

-- | Returns Pictures, consisting of all tile Pictures
renderLevel :: Level -> Picture
renderLevel level = matrixToTilePitures $ layout level
  where
    matrixToTilePitures = pictures . catMaybes . concat . imap rowToTilePictures . reverse
    rowToTilePictures y = imap (`renderTile` y)

renderTile :: Int -> Int -> Tile -> Maybe Picture
renderTile x y tile = case tile of
  Wall              -> Just $ color blue block
  GhostDoor Open    -> Just $ color green block
  GhostDoor Closed  -> Just $ color green block
  _                 -> Nothing
  where
    trans pic = translate (fromIntegral x * tileSize) (fromIntegral y * tileSize) pic
    block = trans (rectangleSolid tileSize tileSize)


renderPlayer :: GameState -> Picture
renderPlayer gs =
  translate x' y' $ color yellow (circleSolid (tileSize/2))
    where
        (_, ly) = layoutSize $ layout $ level gs
        (x, y) = intPosition $ pPosition $ player gs
        x' =  fromIntegral x * tileSize
        y' =  (fromIntegral ly * tileSize) - fromIntegral ((y+1) * round tileSize )




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
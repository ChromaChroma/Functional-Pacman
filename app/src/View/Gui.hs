module View.Gui where

import Model.Game
import Model.Characters
import Model.Items
import Model.Score
import Model.Movement as M
import Model.Level
import Controller.Engine


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game as IO
import Data.Maybe
import Data.List
import Data.List.Index
import Graphics.Gloss.Interface.IO.Game (SpecialKey(KeyEsc), Key (SpecialKey))

screen :: Display
screen = InWindow "Pac Man" windowSize windowOffsetPosition
-- screen = FullScreen

windowSize :: (Int, Int)
windowSize = (1000, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

fps :: Int
fps = 60

tileSize :: Float
tileSize = 32

startRender :: IO ()
startRender =
  play
    screen
    black
    fps
    initialModel
    drawingFunc
    inputHandler
    updateFunc

-- | Initial state of the game at startup
initialModel :: GameState
initialModel = defaultGame

-- | Render game state, aligned from bottom left corner
drawingFunc :: GameState -> Picture
drawingFunc gs = fromBottomLeft $ pictures [
  renderGame gs,
  renderOverlay gs
  ]
  where
    (lx, ly) = layoutSize $ layout $ level gs
    (x, y ) = windowSize
    x' = - fromIntegral(x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

    renderGame gs = pictures[
      renderLevel . level $ gs,
      renderPlayer gs
      -- Render Movable?? ghost and player get rendered same way.
      -- renderGhosts . ghosts $ gs
      -- renderItems . items . level $ gs
      -- PointItems
      ]

    renderOverlay gs = translate 0 (fromIntegral ly *  tileSize) $ pictures[
      -- renderLives . pLives . player $ gs
      -- renderScore . score $ gs
      renderDebug gs
      ]

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

renderDebug :: GameState -> Picture
renderDebug gs = pictures $ stack 0 100 [
  -- translate 0 200 . smallText . show . score $ gs,
  smallText . status $ gs,
  smallText . elapsedTime $ gs,
  smallText . unlives . pLives . player $ gs,
  smallText . direction $ gs,
  smallText . bufDirection $ gs,
  smallText . pPosition . player $ gs
  ]
  where
    smallText :: Show a => a -> Picture
    smallText = color white . scale 0.1 0.1 . text . show

    stack :: Float -> Float -> [Picture] -> [Picture]
    stack _ _ []  = []
    stack x y (l:ls) = translate x y l : stack x (y + 25) ls

-- | Input handling 
inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (SpecialKey KeyDown) IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (SpecialKey KeyRight) IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (SpecialKey KeyLeft) IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'p') IO.Down _ _) gs = pause gs
inputHandler (EventKey (Char 'r') IO.Down _ _) gs = resume gs
inputHandler (EventKey (Char 'q') IO.Down _ _) gs = quit gs
inputHandler _ w = w

-- | Update function ran each iteration
updateFunc :: Float -> GameState -> GameState
updateFunc s gs = let ms = round (s * 1000) in step ms gs
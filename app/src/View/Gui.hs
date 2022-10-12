module View.Gui where

import Controller.Engine
import Controller.MovementController (canMovePerpendicular, formatDecimals)
import Data.Fixed
import Data.List
import Data.List.Index
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game (Key (SpecialKey), SpecialKey (KeyEsc))
import Graphics.Gloss.Interface.IO.Game as IO
import Model.Game
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player hiding (position)
import Model.Score
import Numeric
import View.Config
import View.Debug
import View.Helpers
import View.InfoSection
import View.LevelSection

startRender :: IO ()
startRender =
  play
    screen
    black
    fps
    initialModel
    drawingFunc
    inputHandler
    tickEngine

-- | Initial state of the game at startup
initialModel :: GameState
initialModel = defaultGame

-- | Render game state, aligned from bottom left corner
drawingFunc :: GameState -> Picture
drawingFunc gs = fromBottomLeft $ pictures [renderLevelSection gs, renderInfoSection gs, renderDebug gs]
  where
    (x, y) = windowSize
    x' = - fromIntegral (x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

-- | Input handling
inputHandler :: Event -> GameState -> GameState

-- | Movement with arrow keys
inputHandler (EventKey (SpecialKey KeyUp) IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (SpecialKey KeyDown) IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (SpecialKey KeyRight) IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (SpecialKey KeyLeft) IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'w') IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (Char 's') IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (Char 'd') IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (Char 'a') IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'p') IO.Down _ _) gs = pause gs
inputHandler (EventKey (Char 'r') IO.Down _ _) gs = resume gs
inputHandler (EventKey (Char 'q') IO.Down _ _) gs = quit gs
inputHandler _ gs = gs

-- | Update function ran each iteration
-- | Takes seconds since last update as Float, converts it to milliseconds and passes it to the engine step function
tickEngine :: Float -> GameState -> GameState
tickEngine s gs = let ms = round (s * 1000) in tick ms gs
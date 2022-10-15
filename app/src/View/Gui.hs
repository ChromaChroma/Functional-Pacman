module View.Gui where

import Controller.Engine (movePlayer, pause, quit, resume, tick)
import Controller.MovementController (canMovePerpendicular, formatDecimals)
import Data.List ()
import Data.List.Index ()
import Data.Maybe ()
import Graphics.Gloss (Picture, black, pictures, play, translate)
import Graphics.Gloss.Data.ViewPort ()
import Graphics.Gloss.Interface.IO.Game (Key (SpecialKey), SpecialKey (KeyEsc))
import Graphics.Gloss.Interface.IO.Game as IO
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
  )
import Model.Game (GameState, defaultGame)
import Model.Items ()
import Model.Level ()
import Model.Movement as M (Direction (Down, Left, Right, Up))
import Model.Player ()
import Model.Score ()
import Numeric ()
import View.Animation
import View.Config (fps, screen, tileSize, windowSize)
import View.Debug (renderDebug)
import View.Helpers ()
import View.InfoSection (renderInfoSection)
import View.LevelSection (renderLevelSection)
import View.Overlays (renderOverlay)

startRender :: Textures -> IO ()
startRender textures = do
  play
    screen
    black
    fps
    (initialModel textures)
    drawingFunc
    inputHandler
    tickEngine

data TotalState = TotalState {gameState :: GameState, textures :: Textures}

-- | Initial state of the game at startup
initialModel :: Textures -> TotalState
initialModel t = TotalState {gameState = defaultGame, textures = t}

-- | Render game state, aligned from bottom left corner
drawingFunc :: TotalState -> Picture
drawingFunc ts = pictures (renders : [renderOverlay gs])
  where
    gs = gameState ts
    t = textures ts
    renders =
      fromBottomLeft $
        pictures
          [ renderLevelSection t gs,
            renderInfoSection gs,
            renderDebug gs
          ]
    (x, y) = windowSize
    x' = - fromIntegral (x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

-- | Input handling
inputHandler :: Event -> TotalState -> TotalState

-- | Movement with arrow keys
inputHandler (EventKey (SpecialKey KeyUp) IO.Down _ _) ts = ts {gameState = movePlayer M.Up (gameState ts)}
inputHandler (EventKey (SpecialKey KeyDown) IO.Down _ _) ts = ts {gameState = movePlayer M.Down (gameState ts)}
inputHandler (EventKey (SpecialKey KeyRight) IO.Down _ _) ts = ts {gameState = movePlayer M.Right (gameState ts)}
inputHandler (EventKey (SpecialKey KeyLeft) IO.Down _ _) ts = ts {gameState = movePlayer M.Left (gameState ts)}
inputHandler (EventKey (Char 'w') IO.Down _ _) ts = ts {gameState = movePlayer M.Up (gameState ts)}
inputHandler (EventKey (Char 's') IO.Down _ _) ts = ts {gameState = movePlayer M.Down (gameState ts)}
inputHandler (EventKey (Char 'd') IO.Down _ _) ts = ts {gameState = movePlayer M.Right (gameState ts)}
inputHandler (EventKey (Char 'a') IO.Down _ _) ts = ts {gameState = movePlayer M.Left (gameState ts)}
inputHandler (EventKey (Char 'p') IO.Down _ _) ts = ts {gameState = pause (gameState ts)}
inputHandler (EventKey (Char 'r') IO.Down _ _) ts = ts {gameState = resume (gameState ts)}
inputHandler (EventKey (Char 'q') IO.Down _ _) ts = ts {gameState = quit (gameState ts)}
inputHandler _ ts = ts

-- | Update function ran each iteration
-- | Takes seconds since last update as Float, converts it to milliseconds and passes it to the engine step function
tickEngine :: Float -> TotalState -> TotalState
tickEngine s ts = ts {gameState = newGameState, textures = newTextures}
  where
    newTextures = (textures ts) { elapsedTime = elapsedTime (textures ts) + s }
    newGameState = tick ms (gameState ts)
    ms = round (s * 1000) 
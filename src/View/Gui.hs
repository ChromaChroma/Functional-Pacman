module View.Gui where

import Controller.Engine 
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
    SpecialKey (KeyDown, KeyEnter, KeyLeft, KeyRight, KeyUp),
  )
import Model.Game (GameState (status), Status (..), defaultGame)
import Model.Items ()
import Model.Level ()
import Model.Movement as M (Direction (Down, Left, Right, Up))
import Model.Player ()
import Model.Score ()
import Numeric ()
import View.Animation (Textures (elapsedTime), loadTextures)
import View.Config (framesPerSecond, screen, tileSize, windowSize)
import View.Debug (renderDebug)
import View.Helpers ()
import View.InfoSection (renderInfoSection)
import View.LevelSection (renderLevelSection)
import View.Overlays (renderOverlay)

startRender :: Textures -> IO ()
startRender textures = do
  initialModel <- initModel
  play
    screen
    black
    framesPerSecond
    initialModel
    drawingFunc
    inputDelegationHandler
    tickEngine

data TotalState = TotalState
  { gameState :: GameState,
    textures :: Textures,
    textBuffer :: String
  }

-- | Initial state of the game at startup
initModel :: IO TotalState
initModel = do
  game <- defaultGame
  textures <- loadTextures
  return $ TotalState game textures ""

-- | Render game state, aligned from bottom left corner
drawingFunc :: TotalState -> Picture
drawingFunc ts = pictures (renders : [renderOverlay gs (textBuffer ts)])
  where
    gs = gameState ts
    t = textures ts
    renders =
      fromBottomLeft $
        pictures
          [ renderLevelSection t gs,
            renderInfoSection t gs,
            renderDebug gs
          ]
    (x, y) = windowSize
    x' = - fromIntegral (x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

-- | Input handling
inputDelegationHandler :: Event -> TotalState -> TotalState
inputDelegationHandler event ts
  | (status $ gameState ts) == GameOver = scoreInputHandler event ts
  | otherwise = inputHandler event ts

-- | Handling of typing name for adding score
scoreInputHandler :: Event -> TotalState -> TotalState
scoreInputHandler (EventKey (Char keyCharacter) IO.Down _ _) ts = ts {textBuffer = textBuffer ts ++ [keyCharacter]}
scoreInputHandler (EventKey (SpecialKey KeyEnter) IO.Down _ _) ts = 
  let newGs = submitScore (textBuffer ts) (gameState ts) 
  in ts {textBuffer = [], gameState = newGs} --TODO submit name and reset
scoreInputHandler _ ts = ts

-- | Movement with arrow keys
inputHandler :: Event -> TotalState -> TotalState
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
    newTextures = (textures ts) {elapsedTime = elapsedTime (textures ts) + s}
    newGameState = tick ms (gameState ts)
    ms = round (s * 1000)
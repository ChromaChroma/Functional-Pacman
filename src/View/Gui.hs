module View.Gui where

import Control.Arrow ((***))
import Control.Monad (foldM, join)
import Controller.Engine
import Controller.MovementController (canMovePerpendicular)
import Data.List ()
import Data.Maybe ()
import Graphics.Gloss (Picture, black, pictures, translate)
import Graphics.Gloss.Data.ViewPort ()
import Graphics.Gloss.Interface.IO.Game
import Model.Game (GameState (points, status), Status (..), defaultGame, reset)
import Model.Items ()
import Model.Level ()
import qualified Model.Movement as M (Direction (Down, Left, Right, Up))
import Model.Player ()
import Model.Score ()
import Model.Utils (safeInit)
import Numeric ()
import View.Animation (Textures (elapsedTime), loadTextures)
import View.Buttons
import View.Config (framesPerSecond, screen, tileSize, windowSize)
import View.Debug (renderDebug)
import View.Helpers
import View.InfoSection (renderInfoSection)
import View.LevelSection (renderLevelSection)
import View.Overlays (renderOverlay)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

data TotalState = TotalState
  { gameState :: GameState,
    textures :: Textures,
    textBuffer :: String,
    buttons :: [Button TotalState],
    isDebug :: Bool
  }

-------------------------------------------------------------------------------
-- Main Render function
-------------------------------------------------------------------------------

startRender :: Textures -> IO ()
startRender textures = do
  initialModel <- initModel
  playIO
    screen
    black
    framesPerSecond
    initialModel
    drawingFunc
    inputDelegationHandler
    tickEngine

-------------------------------------------------------------------------------
-- Core Logic Functions
-------------------------------------------------------------------------------

-- | Initial state of the game at startup
initModel :: IO TotalState
initModel = do
  game <- defaultGame
  textures <- loadTextures
  return $ TotalState game textures "" [mkDebugButton] False

-- | Render game state, aligned from bottom left corner
drawingFunc :: TotalState -> IO Picture
drawingFunc ts = return $ pictures (renders : [renderOverlay gs (textBuffer ts)])
  where
    gs = gameState ts
    t = textures ts
    renders =
      fromBottomLeft $
        pictures
          [ renderLevelSection (isDebug ts) t gs,
            renderInfoSection t gs,
            renderDebug (isDebug ts) gs,
            drawButton (head $ buttons ts)
          ]

    (x, y) = windowSize
    x' = - fromIntegral (x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

-- | Input handling
inputDelegationHandler :: Event -> TotalState -> IO TotalState
inputDelegationHandler event ts
  | gsStatus == Waiting = return $ waitingInputHandler event ts
  | gsStatus == GameOver = scoreInputHandler event ts
  | otherwise = gameInputHandler event ts
  where
    gsStatus = status $ gameState ts

-- | Handling input to go from idle to starting the game
waitingInputHandler :: Event -> TotalState -> TotalState
waitingInputHandler (EventKey (SpecialKey KeySpace) Down _ _) ts = ts {gameState = (gameState ts) {status = Active}}
waitingInputHandler _ ts = ts

-- | Handling of typing name for adding score
scoreInputHandler :: Event -> TotalState -> IO TotalState
scoreInputHandler (EventKey (Char keyCharacter) Down _ _) ts = return $ ts {textBuffer = textBuffer ts ++ [keyCharacter]}
scoreInputHandler (EventKey (SpecialKey KeyDelete) Down _ _) ts = return $ ts {textBuffer = safeInit (textBuffer ts)}
scoreInputHandler (EventKey (SpecialKey KeyEnter) Down _ _) ts =
  if not (null (textBuffer ts))
    then do
      newGs <- handle (SubmitScore (textBuffer ts)) (gameState ts)
      return $ ts {textBuffer = "", gameState = reset newGs}
    else return ts
scoreInputHandler _ ts = return ts

-- | Game input handling
gameInputHandler :: Event -> TotalState -> IO TotalState
gameInputHandler (EventKey (SpecialKey KeyUp) Down _ _) ts = handle (Move M.Up) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (SpecialKey KeyDown) Down _ _) ts = handle (Move M.Down) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (SpecialKey KeyRight) Down _ _) ts = handle (Move M.Down) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ts = handle (Move M.Left) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'w') Down _ _) ts = handle (Move M.Up) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 's') Down _ _) ts = handle (Move M.Down) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'd') Down _ _) ts = handle (Move M.Down) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'a') Down _ _) ts = handle (Move M.Left) (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'p') Down _ _) ts = handle Pause (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'r') Down _ _) ts = handle Resume (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (Char 'q') Down _ _) ts = handle Quit (gameState ts) >>= setGameStateIO ts
gameInputHandler (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) ts = checkButtonClick (offsetScreenSize (xPos, yPos)) ts
gameInputHandler _ ts = return ts

setGameStateIO :: TotalState -> GameState -> IO TotalState
setGameStateIO ts gs = return ts {gameState = gs}

offsetScreenSize :: (Float, Float) -> (Float, Float)
offsetScreenSize (x, y) =
  let (w, h) = windowSize
      (w', h') = (fromIntegral (w `div` 2), fromIntegral (h `div` 2))
   in (w' + x, h' + y)

-- | Update function ran each iteration
-- | Takes seconds since last update as Float, converts it to milliseconds and passes it to the engine step function
tickEngine :: Float -> TotalState -> IO TotalState
tickEngine s ts = return $ ts {gameState = newGameState, textures = newTextures}
  where
    newTextures = (textures ts) {elapsedTime = elapsedTime (textures ts) + s}
    newGameState = tick ms (gameState ts)
    ms = round (s * 1000)

-------------------------------------------------------------------------------
-- Other Functions
-------------------------------------------------------------------------------

mkDebugButton :: Button TotalState
mkDebugButton =
  let (w', h') = join (***) fromIntegral windowSize
      (bw, bh) = (200, 40) :: (Float, Float)
   in Button
        { position = (w' / 2 - bw / 2, h' - bh - 20),
          size = (bw, bh),
          label = "Toggle Debug ",
          action = \ts -> return $ ts {isDebug = not $ isDebug ts}
        }

checkButtonClick :: (Float, Float) -> TotalState -> IO TotalState
checkButtonClick pos ts = foldActions ts (buttons ts)
  where
    foldActions :: TotalState -> [Button TotalState] -> IO TotalState
    foldActions ts [] = return ts
    foldActions ts (btn : btns) =
      if not $ isClicked btn pos
        then return ts
        else click btn ts >>= flip foldActions btns

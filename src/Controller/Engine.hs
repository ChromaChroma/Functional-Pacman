module Controller.Engine where

import Controller.MovementController as MC (makePlayerMove)
import Controller.ScoreController
import Model.Game
import Model.GhostAI (makeGhostsMove)
import Model.Ghosts
import Model.Level (isLevelComplete)
import Model.Movement (Direction)
import Model.Player as P (Player (bufDirection, lives), isAlive)
import Prelude hiding (Left, Right)

startNewGame :: GameState
startNewGame = undefined

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | The specified minimal duration between each game tick
tickDuration :: Time
tickDuration = 30

scatterDuration :: Time
scatterDuration = 7000

chaseDuration :: Time
chaseDuration = 20000

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

tick :: Int -> GameState -> GameState
tick ms gs
  | status gs == Active && tickTimer gs + ms > tickDuration =
    resetTickTimer
      . checkGameOver
      . checkLevelComplete
      . checkFruitSpawning
      . checkCollisions
      . checkGhostSpawn
      . makeGhostsMove
      . makePlayerMove
      . addElapsedTime
      $ gs
  | status gs == Active =
    checkScatterMode
      . checkFrightMode
      . addElapsedTime
      $ gs
  | otherwise = gs
  where
    addElapsedTime gs = gs {elapsedTime = elapsedTime gs + ms, tickTimer = tickTimer gs + ms}
    resetTickTimer gs = gs {tickTimer = tickTimer gs `mod` tickDuration}

    checkFrightMode :: GameState -> GameState
    checkFrightMode gs
      | frightenedTime gs >= frightenedDuration && (ghostMode gs == Frightened) = gs {ghostMode = prevGM gs, ghosts = speedGhostsUp (ghosts gs)} -- eerste keer als frightenedtime de duration voorbij is
      | frightenedTime gs >= frightenedDuration && (ghostMode gs /= Frightened) = gs
      | frightenedTime gs < frightenedDuration = frightGen gs {frightenedTime = frightenedTime gs + ms} --generate new seed every time
      | otherwise = gs

    checkScatterMode :: GameState -> GameState
    checkScatterMode gs = case ghostMode gs of
      Scatter ->
        if scatterTime gs >= scatterDuration
          then gs {ghostMode = Chasing, ghosts = turnGhostsAround (ghosts gs), scatterTime = 0}
          else gs {scatterTime = scatterTime gs + ms}
      Chasing ->
        if scatterTime gs >= chaseDuration
          then gs {ghostMode = Scatter, ghosts = turnGhostsAround (ghosts gs), scatterTime = 0}
          else gs {scatterTime = scatterTime gs + ms}
      Frightened -> gs

--------------------------------------------------------------------------------

-- |
-- | Game Input functions
-- |

-- | Change player's direction / stop
movePlayer :: Direction -> GameState -> GameState
movePlayer dir gs = gs {player = (player gs) {bufDirection = dir}}

--  Pause the game
pause :: GameState -> GameState
pause gs
  | status gs == Active = gs {status = Paused}
  | otherwise = gs

-- | Resume the game
resume :: GameState -> GameState
resume gs
  | status gs == Paused = gs {status = Active}
  | otherwise = gs

-- | End the game (forfeiting the current game)
quit :: GameState -> GameState
quit gs = gs {status = GameOver}

-- | Submit name for score
submitScore :: String -> GameState -> IO GameState
submitScore name gs
  | status gs /= GameOver = return $ gs
  | otherwise = do
    let newGs = addScore name gs
    updateScores $ highScores newGs
    return $ newGs

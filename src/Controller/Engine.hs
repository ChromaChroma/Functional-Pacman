module Controller.Engine where

import Controller.MovementController as MC (makePlayerMove)
import Model.Game
  ( GameState (elapsedTime, player, status, tickTimer, level, frightenedTime, ghostMode),
    Status (Active, GameOver, Paused),
    checkCollisions,
    checkGameOver,
    tickDurationIn, frightenedDuration, GhostMode (Scatter), checkFruitSpawning
  )
import Model.Level (isLevelComplete)
import Model.Movement (Direction)
import Model.Player as P (Player (lives, bufDirection), isAlive)
import Model.GhostAI (makeGhostsMove)
import Prelude hiding (Left, Right)

startNewGame :: GameState
startNewGame = undefined

-- | Later in step flow, Add movement buffer, If current direction == stop, then set buffer to current direction

-- | Game step should do all of the following:
-- | Check Game Over
-- | Check for Next level
-- | Check Level Complete
-- | Add life on n score
-- | Check and possibly respawn eaten ghosts
-- | Check and possibly respawn dead player
-- | Update Player state
-- | Update Enemies state
-- | Update Player Movement and check collisions
-- | Update Enemies movement and check collisions
-- | Check for player collision with ghosts
-- | -- | Strong: kill ghost, ghost speed x 5, ghost goes to spawn, ghosts ressurect (in 5 seconds?)
-- | -- | Normal: kill player, do life check
-- | Check and possibly spawn fruit
-- | Update score
-- | Update timer
tick :: Int -> GameState -> GameState
tick ms gs
  | status gs == Active && tickTimer gs + ms > tickDurationIn = do
    resetTickTimer
      . checkGameOver
      . checkLevelComplete
      . checkFruitSpawning
      . checkCollisions
      . updateGhosts
      . updatePlayerMovement
      $ gs
  | status gs == Active = checkGhostMode . addElapsedTime $ gs
  | otherwise = gs
  where
    addElapsedTime gs = gs {elapsedTime = elapsedTime gs + ms, tickTimer = tickTimer gs + ms}
    checkGhostMode gs
      | frightenedTime gs >= frightenedDuration = gs {ghostMode = Scatter} -- TODO: change to time base ghost mode
      | frightenedTime gs < frightenedDuration = gs {frightenedTime = frightenedTime gs + ms}
      | otherwise = gs


-- Check ghost state, if frightened + fTime if Frightened and already == ftime, set normal, if nothing do nothing

checkLevelComplete :: GameState -> GameState
checkLevelComplete gs = if isLevelComplete . level $ gs
  then gs {status = GameOver} --todo: next level / reset current level/maze
  else gs

updatePlayerMovement :: GameState -> GameState
updatePlayerMovement = makePlayerMove

-- | Update ghosts position and state (Chase / Scatter / Frightened)
updateGhosts :: GameState -> GameState
updateGhosts = makeGhostsMove

-- | Reset tick time to 0 for next tick cycle
resetTickTimer :: GameState -> GameState
resetTickTimer gs = gs {tickTimer = 0}

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

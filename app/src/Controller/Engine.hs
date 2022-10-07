module Controller.Engine where

import Model.Player as P
import Model.Game
import Model.Level
import Model.Movement
import Prelude hiding (Left, Right)
import Controller.MovementController as MC


startNewGame :: GameState
startNewGame = undefined

-- | Later in step flow, Add movement buffer, If current direction == stop, then set buffer to current direction

-- | Game step should do all of the following:
-- | Check Game Over
-- | Check for Next level
-- | Check Level Complete
-- | Add life on n score
-- | Check and possibly respawn dead ghosts
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
tick ms gs  | status gs == Active && tickTimer gs + ms > tickDurationInMs = do
                resetTickTimer
                  . checkGameOver
                  . updateGhosts
                  . updatePlayerMovement $ gs
            | status gs == Active = gs { elapsedTime = elapsedTime gs + ms, tickTimer = tickTimer gs + ms }
            | otherwise = gs

updatePlayerMovement :: GameState -> GameState
updatePlayerMovement = makePlayerMove 

-- | Update ghosts position and state (Chase / Scatter / Frightened)
updateGhosts :: GameState -> GameState
updateGhosts gs = gs --todo

-- | Check if game is over and update it if necessary
checkGameOver :: GameState -> GameState
checkGameOver gs
  | isAlive' = gs
  | otherwise = gs {status = Lost} --todo more
  where
    isAlive' = isAlive . P.lives . player $ gs

-- | Reset tick time to 0 for next tick cycle
resetTickTimer :: GameState -> GameState
resetTickTimer gs = gs { tickTimer = 0 }

-- |
-- | Game Input functions
-- |

-- | Change player's direction / stop
movePlayer :: Direction -> GameState -> GameState
movePlayer dir gs = gs { bufDirection = dir }

--  Pause the game
pause :: GameState -> GameState
pause gs  | status gs == Active = gs { status = Paused }
          | otherwise = gs

-- | Resume the game
resume :: GameState -> GameState
resume gs | status gs == Paused = gs { status = Active }
          | otherwise = gs

-- | End the game (forfeiting the current game)
quit :: GameState -> GameState
quit gs = gs { status = Lost }

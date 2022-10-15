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
tick ms gs  | status gs == Active && tickTimer gs + ms > tickDurationIn = do
                resetTickTimer
                  . checkGameOver
                  . checkCollisions
                  . updateGhosts
                  . updatePlayerMovement $ gs
            | status gs == Active = gs { elapsedTime = elapsedTime gs + ms, tickTimer = tickTimer gs + ms }
            | otherwise = gs

updatePlayerMovement :: GameState -> GameState
updatePlayerMovement = makePlayerMove

checkCollisions :: GameState -> GameState
checkCollisions = checkGhostsCollisions
  where
    checkItemCollisions :: GameState -> GameState
    checkItemCollisions gs = map (`removeItem ` gs) ( filter (player gs `collides`) (items . level $ gs))
    removeItem item gs = gs { level = (level gs) { items = filter (/= item) (items . level $ gs) } }

    checkGhostsCollisions gs = if any (player gs `collides`) $ ghosts gs
      then respawnPlayer . reduceLife $ gs
      else gs
    reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
    respawnPlayer gs
      | isAlive . lives $ player gs = gs { player = (player gs) { position = playerSpawn . level $ gs } }
      | otherwise = gs { status = Lost }

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

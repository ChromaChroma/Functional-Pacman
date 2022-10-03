module Controller.Engine where

import Model.Characters as C
import Model.Game
import Model.Level
import Model.Movement


startNewGame :: GameState
startNewGame = undefined

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
-- | -- | Strong: kill ghost, make ai let ghost go to ghost spawn
-- | -- | Normal: kill player, do life check
-- | Check and possibly spawn fruit
-- | Update score
-- | Update timer
step :: Int -> GameState -> GameState
step ms gs  | status gs == Active && elapsedTime gs + ms > tickDurationInMs = do
                -- Game flow ran each tick
                -- Check/Update Player state
                -- Move player
                -- Check/Move Ghosts (AI)
                -- Check game over
                resetElapsedTime
                  . checkGameOver
                  . updateGhosts
                  . updateGhosts
                  . updatePlayer $ gs
            | otherwise = gs { elapsedTime = elapsedTime gs + ms }



-- | Update player position and state (Normal/Strong)
updatePlayer :: GameState -> GameState
updatePlayer gs = gs { player = player' }
  where
    player'
      | validPlayerMove movedPlayer gs = movedPlayer
      | otherwise = player gs { direction = Stop }
    movedPlayer = C.move (player gs) (direction gs)


validPlayerMove :: Player -> GameState -> Bool
validPlayerMove = isValidMove isValid
  where
    isValid Wall = False
    isValid (GhostDoor _) = False
    isValid _ = True

validGhostMove :: Ghost -> GameState -> Bool
validGhostMove = isValidMove isValid
  where
    isValid Wall = False
    isValid (GhostDoor Open) = False
    isValid (GhostDoor Closed) = True
    isValid _ = True

isValidMove :: Movable m => (Tile -> Bool) -> m -> GameState -> Bool
isValidMove f m gs = f $ tileAt (level gs) (intPosition $ getPosition m)

-- | Update ghosts position and state (Chase / Scatter / Frightened)
updateGhosts :: GameState -> GameState
updateGhosts gs = gs --todo

-- | Check if game is over and update it if necessary
checkGameOver :: GameState -> GameState
checkGameOver gs = gs --todo

-- | Reset elapsed time to 0 for next tick cycle
resetElapsedTime :: GameState -> GameState
resetElapsedTime gs = gs { elapsedTime = 0 }

-- |
-- | Game Input functions
-- |

-- | Change player's direction / stop
movePlayer :: Direction -> GameState -> GameState
movePlayer dir gs = gs { direction = dir }

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

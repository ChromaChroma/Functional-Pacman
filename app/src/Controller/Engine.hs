module Controller.Engine where

import Model.Characters as C
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
step :: Int -> GameState -> GameState
step ms gs  | status gs == Active && elapsedTime gs + ms > tickDurationInMs = do
                resetElapsedTime
                  . checkGameOver
                  . updateGhosts
                  . updatePlayerMovement $ gs
            | status gs == Active = gs { elapsedTime = elapsedTime gs + ms }
            | otherwise = gs


updatePlayerMovement :: GameState -> GameState
updatePlayerMovement = makePlayerMove 
  -- -- | isValidBufferMove = gs {player = bufMovedPlayer, direction = bufDirection gs, bufDirection = Stop}
  -- | isValidNormalMove = gs {player = movedPlayer}
  -- | otherwise = gs { direction = Stop }
  -- where
  --   isValidNormalMove = 
            

-- updatePlayerMovement :: GameState -> GameState
-- updatePlayerMovement gs
--   | isValidBufferMove = gs {player = bufMovedPlayer, direction = bufDirection gs, bufDirection = Stop}
--   | isValidNormalMove = gs {player = movedPlayer}
--   | otherwise = gs { direction = Stop }
--   where
--     isValidBufferMove = bufDirection gs /= Stop && validateMove (bufDirection gs) gs
--     isValidNormalMove = direction gs /= Stop    && validateMove (direction gs   ) gs

--     validateMove dir gs = validPlayerMove (moveFull (player gs) dir) gs

--     movedPlayer     = roundToAxis $ movePlayer (direction gs)
--     bufMovedPlayer  = roundToAxis $ movePlayer (bufDirection gs)
--     movePlayer dir  = C.move (player gs) dir

--     -- | Wachy fix for player movement by locking perpendicular axis of current directino
--     roundToAxis :: Player -> Player
--     roundToAxis p@Player{pPosition = (x, y)} = case direction gs of
--       Up -> p {pPosition = (fromIntegral $ round x, y)}
--       Down -> p {pPosition = (fromIntegral $ round x, y)}
--       Left -> p {pPosition = (x, fromIntegral $ round y)}
--       Right -> p {pPosition = (x, fromIntegral $ round y)}
--       _ -> p {pPosition = (fromIntegral $ round x, fromIntegral $ round y)}

validPlayerMove :: Player -> GameState -> Bool
validPlayerMove = isValidMove isValid
  where
    isValid Wall = False
    isValid (GhostDoor _) = False
    isValid _ = True

-- validGhostMove :: Ghost -> GameState -> Bool
-- validGhostMove = isValidMove isValid
--   where
--     isValid Wall = False
--     isValid (GhostDoor Open) = False
--     isValid (GhostDoor Closed) = True
--     isValid _ = True
moveFull :: Movable a => a -> Direction -> a
moveFull m dir = setPosition m (moveFullUnit m dir)
  where 
    moveFullUnit m dir = case dir of
      Up    -> (x,y-1)
      Down  -> (x,y+1)
      Left  -> (x-1,y)
      Right -> (x+1,y)
      _     -> (x,y)
      where (x, y) = getPosition m

isValidMove :: Movable m => (Tile -> Bool) -> m -> GameState -> Bool
isValidMove f m gs = case tileAt (level gs) (intPosition $ getPosition m) of
    Just a  -> f a
    _       -> False  

-- | Update ghosts position and state (Chase / Scatter / Frightened)
updateGhosts :: GameState -> GameState
updateGhosts gs = gs --todo

-- | Check if game is over and update it if necessary
checkGameOver :: GameState -> GameState
checkGameOver gs
  | isAlive' = gs
  | otherwise = gs {status = Lost} --todo more
  where
    isAlive' = isAlive . C.pLives . player $ gs

-- | Reset elapsed time to 0 for next tick cycle
resetElapsedTime :: GameState -> GameState
resetElapsedTime gs = gs { elapsedTime = 0 }

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

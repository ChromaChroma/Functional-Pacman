module Controller.Engine where

import Controller.ScoreController
import Controller.MovementController as MC (makePlayerMove)
import Model.Game
import Model.Level (isLevelComplete)
import Model.Movement (Direction)
import Model.Player as P (Player (lives, bufDirection), isAlive)
import Model.Ghosts
import Model.Game
import Model.GhostAI (makeGhostsMove)
import Prelude hiding (Left, Right)

startNewGame :: GameState
startNewGame = undefined

tick :: Int -> GameState -> GameState
tick ms gs
  | status gs == Active && tickTimer gs + ms > tickDurationIn =
    resetTickTimer
      . checkGameOver
      . checkLevelComplete
      . checkFruitSpawning
      . checkCollisions
      . checkGhostSpawn
      . updateGhosts
      . updatePlayerMovement
      $ gs
  | status gs == Active = checkScatterMode . checkFrightMode . addElapsedTime $ gs
  | otherwise = gs
  where
    addElapsedTime gs = gs {elapsedTime = elapsedTime gs + ms, tickTimer = tickTimer gs + ms}

    checkFrightMode :: GameState -> GameState
    checkFrightMode gs
      | frightenedTime gs >= frightenedDuration && (ghostMode gs == Frightened) = gs {ghostMode = prevGM gs, ghosts = speedGhostsUp (ghosts gs)} -- eerste keer als frightenedtime de duration voorbij is
      | frightenedTime gs >= frightenedDuration && (ghostMode gs /= Frightened) = gs
      | frightenedTime gs < frightenedDuration = frightGen gs {frightenedTime = frightenedTime gs + ms} --generate new seed every time
      | otherwise = gs

    checkScatterMode :: GameState -> GameState
    checkScatterMode gs =
      case ghostMode gs of
        Scatter -> case (scatterTime gs >= scatterDuration) of
                    True  -> gs {ghostMode = Chasing, ghosts = turnGhostsAround (ghosts gs), scatterTime = 0}
                    False -> gs {scatterTime = scatterTime gs + ms}
        Chasing -> case (scatterTime gs >= chaseDuration) of
                    True  -> gs {ghostMode = Scatter, ghosts = turnGhostsAround (ghosts gs), scatterTime = 0}
                    False -> gs {scatterTime = scatterTime gs + ms}
        Frightened -> gs
      where
        scatterDuration = 7000
        chaseDuration = 20000

--------------------------------------------------------------------------------

checkLevelComplete :: GameState -> GameState
checkLevelComplete gs
  | isLevelComplete . level $ gs = loadNextLevel gs
  | otherwise = gs

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

-- | Submit name for score
submitScore :: String -> GameState -> IO GameState
submitScore name gs
  | status gs /= GameOver = return $ gs
  | otherwise = do
      let newGs = addNewScore name gs
      updateScores $ highScores newGs
      return $ newGs

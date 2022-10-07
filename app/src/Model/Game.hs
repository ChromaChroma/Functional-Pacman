module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    Time,
    tickDurationInMs,
  )
where

import Model.Ghosts (Ghost, blinky, clyde, inky, pinky)
import Model.Level (Level(playerSpawn), defaultLevel)
import Model.Movement (Direction (..), Positioned(setPosition))
import Model.Player (Player, defaultPlayer)
import Prelude hiding (Left, Right)

-- | Time a game has been running
type Time = Int

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState
  { status :: Status,
    player :: Player,
    level :: Level,
    elapsedTime :: Time,
    direction :: Direction,
    bufDirection :: Direction,
    ghosts :: [Ghost]
    -- score       :: Score
  }

loadGame lvl ghosts pl = GameState
    { status = Active,
      elapsedTime = 0,
      direction = Stop,
      bufDirection = Stop,
      player = pl,
      level = lvl,
      ghosts = ghosts
      -- score       = 0
    }
    
defaultGame :: GameState
defaultGame = loadGame lvl ghosts pl
    where
      lvl = defaultLevel
      pl = setPosition defaultPlayer (playerSpawn lvl)
      ghosts = [blinky, pinky, inky, clyde]

-- | Game tick duration in milliseconds
tickDurationInMs :: Int
tickDurationInMs = 30
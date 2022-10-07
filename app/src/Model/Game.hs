module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    Time,
    tickDurationInMs,
  )
where

import Model.Player (Player, defaultPlayer)
import Model.Ghosts (Ghost, blinky, clyde, inky, pinky)
import Model.Level (Level, defaultLevel)
import Model.Movement (Direction (..))
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

defaultGame :: GameState
defaultGame =
  GameState
    { status = Active,
      player = defaultPlayer,
      level = defaultLevel,
      elapsedTime = 0,
      direction = Stop,
      bufDirection = Stop,
      ghosts = [blinky, pinky, inky, clyde]
      -- score       = 0
    }

-- | Game tick duration in milliseconds
tickDurationInMs :: Int
tickDurationInMs = 30
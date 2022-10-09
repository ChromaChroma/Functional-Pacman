module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    Time,
    tickDurationIn,
  )
where

import Model.Ghosts (Ghost, blinky, clyde, inky, pinky)
import Model.Level (Level (playerSpawn), defaultLevel)
import Model.Movement (Direction (..), Positioned (setPosition))
import Model.Player (Player, defaultPlayer)
import Prelude hiding (Left, Right)

-- | Time the game or the tickTimer has been running in milliseconds
type Time = Int

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState
  { status :: Status,
    player :: Player,
    level :: Level,
    elapsedTime :: Time,
    tickTimer :: Time,
    direction :: Direction,
    bufDirection :: Direction,
    ghosts :: [Ghost]
  }

loadGame :: Level -> [Ghost] -> Player -> GameState
loadGame lvl ghosts pl =
  GameState
    { status = Active,
      elapsedTime = 0,
      tickTimer = 0,
      direction = Stop,
      bufDirection = Stop,
      player = pl,
      level = lvl,
      ghosts = ghosts
    }

defaultGame :: GameState
defaultGame = loadGame lvl ghosts pl
  where
    lvl = defaultLevel
    pl = setPosition defaultPlayer (playerSpawn lvl)
    ghosts = [blinky, pinky, inky, clyde]

-- | The specified minimal duration between each game tick

tickDurationIn :: Time
tickDurationIn = 30
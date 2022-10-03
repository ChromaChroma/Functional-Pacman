module Model.Game(
  GameState(..), defaultGame,
  Status(..), 
  Time, 
  tickDurationInMs
  ) where
  
import Model.Characters(Ghost, blinky, pinky, inky, clyde, Player, defaultPlayer, Direction(..))
import Model.Level(Level, defaultLevel)

-- | Time a game has been running
type Time = Int 

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState {
    status      :: Status,
    player      :: Player,
    level       :: Level,
    elapsedTime :: Time,
    direction   :: Direction,
    bufDirection:: Direction,
    ghosts      :: [Ghost]
    -- score       :: Score
}

defaultGame :: GameState
defaultGame = GameState {
    status      = Active,
    player      = defaultPlayer,
    level       = defaultLevel,
    elapsedTime = 0,
    direction   = Right,
    bufDirection= Up,
    ghosts      = [blinky, pinky, inky, clyde]
    -- score       = 0
}

-- | Game tick duration in milliseconds
tickDurationInMs :: Int
tickDurationInMs = 100
module Model.Game(
  GameState(..), newGame,
  Status(..), 
  Time, 
  tickDurationInMs
  ) where
  
import Model.Characters(Player, Ghost)
import Model.Level(Level, defaultLevel)

-- | Game tick duration in milliseconds
tickDurationInMs :: Int
tickDurationInMs = 100

-- | Time a game has been running
type Time = Int 

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Won | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState {
    status      :: Status,
    player      :: Player,
    level       :: Level,
    elapsedTime :: Time
}

-- | Contstructor for a new game
newGame :: Level -> Player -> GameState
newGame level player = GameState {
    status = Waiting,
    player = player,
    level = defaultLevel,
    elapsedTime = 0
  }

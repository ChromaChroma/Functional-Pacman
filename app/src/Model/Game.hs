module Model.Game(
  GameState(..), newGame,
  Status(..), 
  Time, 
  tickDurationInMs
  ) where
  
import Model.Characters(Ghost, blinky, pinky, inky, clyde, Player, defaultPlayer, Direction(..))
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
    elapsedTime :: Time,
    direction   :: Direction,

    ghosts      :: [Ghost]
    -- score       :: Score
    
    
}

defaultGame :: GameState
defaultGame = GameState {
    status      = Active,
    player      = defaultPlayer,
    level       = defaultLevel,
    elapsedTime = 0,
    direction   = Stop,
    ghosts      = [blinky, pinky, inky, clyde]
    -- score       = 0
}

-- | Contstructor for a new game
newGame :: Level -> Player -> GameState
newGame level player = GameState {
    status = Active,
    player = player,
    level = defaultLevel,
    elapsedTime = 0,
    direction   = Stop,
    ghosts = []
  }
  

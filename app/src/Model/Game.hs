module Model.Game(
  GameState, 
  Status, 
  Time, 
  simpleNewGame
  ) where
  
import Model.Player(Player, Enemy)
import Model.Level(Level)

-- | Time a game has been running
data Time = Time {
    time :: Int
}
-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Won | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState {
    status  :: Status
    player  :: Player
    level   :: Level
    time    :: Time
    lives   :: Lives
}

-- | Contstructor for a new game
simpleNewGame :: Level -> Player -> GameState
simpleNewGame level player = GameState {
    status = Waiting
    player = level
    level = player
    time = 0
    lives = mkLives 3
  }

module Model.Characters(
    Name, Direction(..), Speed,
    Movable(..),
    Ghost(..), GhostState, blinky, pinky, inky, clyde,
    Player(..), PlayerState, defaultPlayer,
    Lives(..), LifeState, mkLives, rmLife, isAlive
) where 

import Model.Items (Position, Positioned)
import Model.Movement(Movable(..), Speed, Direction(..))

-- |
-- |  Lives
-- |

-- | Number of lives the player has left
newtype Lives = Lives { 
    unlives :: Int
  }
  deriving (Eq, Show)

-- | Safe Lives constructor
mkLives :: Int -> Maybe Lives
mkLives lives
  | lives > 0 = Just (Lives lives)
  | otherwise = Nothing

-- | Remove a life from the Player's lives
rmLife :: Lives -> Lives
rmLife lives
  | life > 0 = lives {unlives = life - 1}
  | otherwise = lives {unlives = life - 1}
  where
    life = unlives lives

isAlive :: Lives -> Bool
isAlive = (> 0) . unlives

-- |
-- |  Player
-- |

-- | A Player's state
-- | Normal is the players default state
-- | Strong is the state the player is in when he eats a power pellet and when he can attack the ghosts
data PlayerState = Normal | Strong deriving (Eq, Show)

data Player = Player { 
    playerState :: PlayerState,
    pPosition :: Position,
    pSpeed :: Speed,
    pLives :: Lives,
    pDirection :: Direction
  } deriving (Eq)

-- | The player's Movable implementation
instance Movable Player where
  getSpeed = pSpeed
  getPosition = pPosition
  setPosition player pos = player {pPosition = pos}

defaultPlayer :: Player
defaultPlayer = Player Normal (14.5, 23) 0.1 (Lives 3) Stop

-- |
-- |  Ghosts
-- |

-- | State of living of a Ghost
data LifeState = Alive | Dead deriving (Eq, Show)

-- | Name of a Ghost
type Name = String

-- | States a ghost can be in
-- | Chasing is the state in which ghosts chase the player
-- | Frightend is the state in which ghosts run away from the player
-- | Scatter is the state in which ghosts move to a specific location
data GhostState = Chasing | Frightend | Scatter deriving (Eq, Show)

-- | The ghost's current state
data Ghost = Ghost{ 
    gName :: Name,
    mode :: GhostState,
    gPosition :: Position,
    gSpeed :: Speed,
    gAlive :: LifeState
  } deriving (Eq)

-- | The ghost's Movable implementation
instance Movable Ghost where
  getSpeed = gSpeed
  getPosition = gPosition
  setPosition ghost pos = ghost {gPosition = pos}

-- | Default ghost constructors for each original ghost

blinky :: Ghost
blinky = Ghost "Blinky" Scatter (5,5) 0.1 Alive

pinky :: Ghost
pinky = Ghost "Pinky" Scatter (5,6) 0.1 Alive

inky :: Ghost
inky = Ghost "Inky" Scatter (6,5) 0.1 Alive

clyde :: Ghost
clyde = Ghost "Clyde" Scatter (6,6) 0.1 Alive


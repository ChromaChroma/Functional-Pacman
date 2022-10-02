module Model.Characters(
    Name, Direction(..), Speed,
    Movable(..),
    Ghost(..), GhostState, blinky, pinky, inky, clyde,
    Player(..), PlayerState, defaultPlayer,
    Lives(..), LifeState, mkLives, rmLife,
) where

import Prelude hiding (Left, Right)
import Model.Items (Position, Positioned)

-- | Directions a movement can be in, including Stop for an idle in movement
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)

-- | Name of a Player or Ghost
type Name = String

-- | A movable's Speed in Ints unit
type Speed = Float

-- -- | A movable's position in Floats
-- type Position = (Float, Float) -- Floats might be prefered

class Movable a where 
    getSpeed :: a -> Speed
    getPosition :: a -> Position
    setPosition :: a -> Position -> a
    move :: a -> Direction -> a
    move movable direction = setPosition movable (x', y')
        where
            (x, y) = getPosition movable
            s = getSpeed movable
            (x', y') = case direction of
                Up -> (x, y + s)
                Down -> (x, y - s)
                Left -> (x - s, y)
                Right -> (x + s, y)
                Stop -> (x, y)

-- | State of living of a Player or Ghost
data LifeState = Alive | Dead deriving (Eq, Show)

-- | Number of lives the player has left
data Lives = Lives
  { amount :: Int,
    alive :: LifeState
  }
  deriving (Eq, Show)

-- | Safe Lives constructor
mkLives :: Int -> Maybe Lives
mkLives lives
  | lives > 0 = Just (Lives lives Alive)
  | otherwise = Nothing

-- | Remove a life from the Player's lives
rmLife :: Lives -> Lives
rmLife lives
  | life > 0 = lives {amount = life - 1}
  | otherwise = lives {amount = life - 1, alive = Dead}
  where
    life = amount lives

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
defaultPlayer = Player Normal (1, 4) 0.1 (Lives 3 Alive) Stop

-- | States a ghost can be in
-- | Chasing is the state in which ghosts chase the player
-- | Frightend is the state in which ghosts run away from the player
-- | Scatter is the state in which ghosts move to a specific location
data GhostState = Chasing | Frightend | Scatter deriving (Eq, Show) -- of Vulnerable

-- | The ghost's current state
data Ghost = Ghost
  { gName :: Name,
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

blinky :: Ghost
blinky = Ghost "Blinky" Scatter (5,5) 0.1 Alive

pinky :: Ghost
pinky = Ghost "Pinky" Scatter (5,6) 0.1 Alive

inky :: Ghost
inky = Ghost "Inky" Scatter (6,5) 0.1 Alive

clyde :: Ghost
clyde = Ghost "Clyde" Scatter (6,6) 0.1 Alive


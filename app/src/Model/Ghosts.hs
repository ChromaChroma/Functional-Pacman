module Model.Ghosts
  ( Ghost (..),
    GhostState(..),
    Name(..),
    LifeState(..),
    blinky,
    pinky,
    inky,
    clyde,
  )
where

import Model.Movement (Collidable, Direction (..), Movable (..), Position, Positioned (..), Speed)

-- | State of living of a Ghost
data LifeState = Alive | Eaten deriving (Eq, Show)

-- | Name of a Ghost
data Name = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

-- | States a ghost can be in
-- | Chasing is the state in which ghosts chase the player
-- | Frightened is the state in which ghosts run away from the player
-- | Scatter is the state in which ghosts move to their specific location 
data GhostState = Chasing | Frightened | Scatter deriving (Eq, Show)

-- | The ghost's current state
data Ghost = Ghost
  { name :: Name,
    mode :: GhostState,
    position :: Position,
    speed :: Speed,
    lifeState :: LifeState,
    direction :: Direction,
    prevDirection :: Direction
  } deriving (Eq)

instance Positioned Ghost where
  getPosition = position
  setPosition ghost pos = ghost {position = pos}
  
instance Collidable Ghost

instance Movable Ghost where
  getSpeed = speed

-- | Default ghost constructors for each original ghost
blinky :: Ghost
blinky = Ghost Blinky Scatter (12, 16) 0.1 Alive Stop Stop

pinky :: Ghost
pinky = Ghost Pinky Scatter (13, 16) 0.1 Alive Stop Stop

inky :: Ghost
inky = Ghost Inky Scatter (14, 16) 0.1 Alive Stop Stop

clyde :: Ghost
clyde = Ghost Clyde Scatter (15, 16) 0.1 Alive Stop Stop

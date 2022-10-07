module Model.Ghosts
  ( Ghost (..),
    GhostState,
    Name,
    LifeState,
    blinky,
    pinky,
    inky,
    clyde,
  )
where

import Model.Movement (Direction (..), Movable (..), Position, Positioned (..), Speed)

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
data Ghost = Ghost
  { gName :: Name,
    mode :: GhostState,
    gPosition :: Position,
    gSpeed :: Speed,
    gAlive :: LifeState
  }
  deriving (Eq)

instance Positioned Ghost where
  getPosition = gPosition
  setPosition ghost pos = ghost {gPosition = pos}

instance Movable Ghost where
  getSpeed = gSpeed

-- | Default ghost constructors for each original ghost
blinky :: Ghost
blinky = Ghost "Blinky" Scatter (12, 14) 0.1 Alive

pinky :: Ghost
pinky = Ghost "Pinky" Scatter (13, 14) 0.1 Alive

inky :: Ghost
inky = Ghost "Inky" Scatter (14, 14) 0.1 Alive

clyde :: Ghost
clyde = Ghost "Clyde" Scatter (15, 14) 0.1 Alive

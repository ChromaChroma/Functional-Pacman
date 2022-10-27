module Model.Ghosts
  ( Ghost (..),
    Name (..),
    EatenState (..),
    blinky,
    pinky,
    inky,
    clyde,
    isEaten,
    isNotEaten,
    collidesWithMovable,
  )
where

import Model.Movement (Collidable (collides), Direction (..), Movable (..), Position, Positioned (..), Speed)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | State of living of a Ghost
data EatenState = NotEaten | Eaten deriving (Eq, Show)

-- | Name of a Ghost
data Name = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

-- | The ghost's current state
data Ghost = Ghost
  { name :: Name,
    position :: Position,
    speed :: Speed,
    eatenState :: EatenState,
    direction :: Direction,
    prevDirection :: Direction
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned Ghost where
  getPosition = position
  setPosition ghost pos = ghost {position = pos}

instance Collidable Ghost

instance Movable Ghost where
  getSpeed = speed

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

isEaten :: Ghost -> Bool
isEaten = (== Eaten) . eatenState

isNotEaten :: Ghost -> Bool
isNotEaten = not . isEaten

collidesWithMovable :: (Movable a, Collidable a) => Ghost -> a -> Bool
collidesWithMovable ghost m = isNotEaten ghost && ghost `collides` m

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

-- | Default ghost constructors for each original ghost
blinky :: Ghost
blinky = Ghost Blinky (12, 16) 0.1 NotEaten Stop Stop

pinky :: Ghost
pinky = Ghost Pinky (13, 16) 0.01 NotEaten Up Stop

inky :: Ghost
inky = Ghost Inky (14, 16) 0.1 NotEaten Stop Stop

clyde :: Ghost
clyde = Ghost Clyde (15, 16) 0.1 NotEaten Stop Stop

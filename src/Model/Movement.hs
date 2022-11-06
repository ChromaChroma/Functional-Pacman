module Model.Movement
  ( Positioned (..),
    Position,
    intPosition,
    Collidable(..),
    Movable (..),
    Direction (..),
    Speed,
  )
where

import Model.Utils (mod')
import Prelude hiding (Down, Left, Right, Up)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Position of something
type Position = (Float, Float)

-- | Directions a movement can be in, including Stop for an idle in movement
data Direction = Up | Left | Down | Right | Stop deriving (Eq, Ord, Show)

-- | A movable's Speed in Ints unit
type Speed = Float

-------------------------------------------------------------------------------
-- Type classes
-------------------------------------------------------------------------------

-- | Something that has a position
class Positioned a where
  getPosition :: a -> Position
  setPosition :: a -> Position -> a

-- | Class to check if a Positioned collides with another Positioned
class (Positioned a) => Collidable a where
  collides :: Collidable b =>  a -> b -> Bool
  a `collides` b = checkPositions (getPosition a) (getPosition b)
    where
      checkPositions (ax, ay) (bx, by) = isWithinThreshhold ax bx && isWithinThreshhold ay by
      isWithinThreshhold z z' = abs (z - z') <= threshhold
      threshhold = 0.1 -- 0.1 is a constant deviation from another collidable that would count as collision

class (Positioned a) => Movable a where
  getSpeed :: a -> Speed
  move :: a -> Direction -> (Int, Int) -> a
  -- Default implementation
  move movable direction (w, h) = setPosition movable $ wrapMovement (x', y')
    where
      (x, y) = getPosition movable
      s = getSpeed movable
      (x', y') = case direction of
        Up -> (x, y + s)
        Down -> (x, y - s)
        Left -> (x - s, y)
        Right -> (x + s, y)
        Stop -> (x, y)
      wrapMovement :: Position -> Position
      wrapMovement (x, y) = (x `mod'` (fromIntegral w-1), y `mod'` (fromIntegral h - 1))

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

intPosition :: Position -> (Int, Int)
intPosition (x, y) = (round x, round y)

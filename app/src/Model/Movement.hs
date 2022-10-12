module Model.Movement
  ( Positioned (..),
    Position,
    intPosition,
    Movable (..),
    Direction (..),
    Speed,
  )
where
 
import Model.Utils (mod')
import Prelude hiding (Down, Left, Right, Up)

-- | Position of something
type Position = (Float, Float)

intPosition :: Position -> (Int, Int)
intPosition (x, y) = (round x, round y)

-- | Something that has a position
class Positioned a where
  getPosition :: a -> Position
  setPosition :: a -> Position -> a

-- | Directions a movement can be in, including Stop for an idle in movement
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)

-- | A movable's Speed in Ints unit
type Speed = Float

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
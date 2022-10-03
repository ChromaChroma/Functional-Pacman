module Model.Movement(
  Movable(..),
  Direction(..), Speed,
  Positioned(..), Position
) where
  
import Prelude hiding (Left, Right,Up, Down)

-- | Position of something
type Position = (Float, Float)

-- | Wrapper type of 'Position' for 'Movable' a
data Positioned a = Positioned a Position


-- | Directions a movement can be in, including Stop for an idle in movement
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)

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
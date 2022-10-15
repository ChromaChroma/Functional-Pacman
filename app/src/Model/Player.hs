module Model.Player
  ( Speed,
    Movable (..),
    Player (..),
    PlayerState,
    defaultPlayer,
    Lives (..),
    mkLives,
    rmLife,
    isAlive,
  )
where

import Model.Movement (Collidable, Direction (..), Movable (..), Position, Positioned (..), Speed)

-- |
-- |  Lives
-- |

-- | Number of lives the player has left
newtype Lives = Lives
  { unlives :: Int
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
-- | Strong is the state after the player eats a power pellet, in which the player can eat ghosts
data PlayerState = Normal | Strong deriving (Eq, Show)

data Player = Player
  { playerState :: PlayerState,
    position :: Position,
    speed :: Speed,
    lives :: Lives
  }
  deriving (Eq)

instance Positioned Player where
  getPosition = position
  setPosition player pos = player {position = pos}

instance Collidable Player

instance Movable Player where
  getSpeed = speed
  

defaultPlayer :: Player
defaultPlayer = Player Normal (1, 1) 0.1 (Lives 3)
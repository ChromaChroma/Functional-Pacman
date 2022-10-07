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

import Model.Movement (Direction (..), Movable (..), Position, Positioned (..), Speed)

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
-- | Strong is the state the player is in when he eats a power pellet and when he can attack the ghosts
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

instance Movable Player where
  getSpeed = speed

defaultPlayer :: Player
defaultPlayer = Player Normal (14.5, 23) 0.1 (Lives 3)
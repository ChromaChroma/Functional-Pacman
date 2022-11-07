module Model.Player
  ( Speed,
    Movable (..),
    Player (..),
    Lives (..),
    mkLives,
    rmLife,
    isAlive,
  )
where

import Model.Movement (Collidable, Direction (..), Movable (..), Position, Positioned (..), Speed)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Number of lives the player has left
newtype Lives = Lives {unlives :: Int} deriving (Eq, Show)

-- | Player data structure containing all information about the player and its movement
data Player = Player
  { position :: Position,
    speed :: Speed,
    lives :: Lives,
    direction :: Direction,
    bufDirection :: Direction
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned Player where
  getPosition = position
  setPosition player pos = player {position = pos}

instance Collidable Player

instance Movable Player where
  getSpeed = speed

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

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

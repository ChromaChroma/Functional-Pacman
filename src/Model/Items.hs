module Model.Items
  ( PointItem (..),
    mkDot,
    mkPowerPellet,
    FruitType (..),
    Positioned (..),
    Position (..),
  )
where

import Model.Movement (Collidable, Position, Positioned (..))
import Model.Score (Points)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Types of fruit
data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key deriving (Eq, Show)

-- | Item representing an object that can give point score to the player when eaten
-- | Dot          : is the most common item to be consumed and are needed to complete a level
-- | PowerPellet  : is a special item that gives the player the ability to eat ghosts
-- | Fruit        : is a special item that gives the player a large amount of points
data PointItem
  = Dot {position :: Position, points :: Points}
  | PowerPellet {position :: Position, points :: Points}
  | Fruit {position :: Position, points :: Points, itemType :: FruitType}
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned PointItem where
  getPosition = position
  setPosition item newPos = item {position = newPos}

instance Collidable PointItem

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

mkDot :: Position -> PointItem
mkDot pos = Dot pos 10

mkPowerPellet :: Position -> PointItem
mkPowerPellet pos = PowerPellet pos 50

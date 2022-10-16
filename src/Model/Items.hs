module Model.Items
  ( PointItem (..),
    mkDot,
    mkPowerPellet,
    FruitType,
    defaultFruits,
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
  = Dot
      { position :: Position,
        points :: Points
      }
  | PowerPellet
      { position :: Position,
        points :: Points
      }
  | Fruit
      { position :: Position,
        itemType :: FruitType,
        points :: Points
      }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned PointItem where
  getPosition = position
  setPosition item _ = item -- PointItems By default should nog change position, so setPosition returns the passed data

instance Collidable PointItem

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

mkDot :: Position -> PointItem
mkDot pos = Dot pos 10

mkPowerPellet :: Position -> PointItem
mkPowerPellet pos = PowerPellet pos 50

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

defaultFruits :: [PointItem]
defaultFruits =
  [ Fruit (0, 0) Cherry 100,
    Fruit (0, 0) Strawberry 300,
    Fruit (0, 0) Orange 500,
    Fruit (0, 0) Apple 700,
    Fruit (0, 0) Melon 1000,
    Fruit (0, 0) Galaxian 2000,
    Fruit (0, 0) Bell 3000,
    Fruit (0, 0) Key 5000
  ]

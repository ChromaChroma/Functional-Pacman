module Model.Items(
  PointItem(..),
  FruitType, defaultFruits,
  Positioned(..), Position(..)
) where

import Model.Movement(Position, Positioned(..))

-- | Score points
type Points = Int

-- | Item representing an object that can give point score to the player when eaten
-- | Dot is the most common item to be consumed and are needed to complete a level
-- | PowerPellet is a special item that gives the player the ability to eat ghosts
-- | Fruit is a special item that gives the player a large amount of points
data PointItem = 
    Dot {
      position :: Position,
      points :: Points
    } 
  | PowerPellet {
      position :: Position,
      points :: Points
      } 
  | Fruit {
      position :: Position,
      itemType :: FruitType,
      points :: Points
    } 
  deriving (Eq)

instance Positioned PointItem where
  getPosition = position
  -- | PointItems By default should nog change position, so setPosition returns the passed item
  setPosition item _ = item

-- | Types of fruit
data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key deriving (Eq, Show)

defaultFruits :: [PointItem]
defaultFruits = [
  Fruit (0,0) Cherry 100,
  Fruit (0,0) Strawberry 300,
  Fruit (0,0) Orange 500,
  Fruit (0,0) Apple 700,
  Fruit (0,0) Melon 1000,
  Fruit (0,0) Galaxian 2000,
  Fruit (0,0) Bell 3000,
  Fruit (0,0) Key 5000
  ]

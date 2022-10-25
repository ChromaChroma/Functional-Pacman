module Model.Items
  ( PointItem (..),
    mkDot,
    mkPowerPellet,
    FruitType(..),
    defaultFruits,
    Positioned (..),
    Position (..),
    fruitOfLevel,
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
        points :: Points,
        itemType :: FruitType
      }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned PointItem where
  getPosition = position
  setPosition item newPos = item { position = newPos}

instance Collidable PointItem

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

mkDot :: Position -> PointItem
mkDot pos = Dot pos 10

mkPowerPellet :: Position -> PointItem
mkPowerPellet pos = PowerPellet pos 50

-- | Takes the level number and spawns the corresponding fruit type
-- | From level 8 and on the fruit type is key
fruitOfLevel :: Int -> PointItem
fruitOfLevel n
  | n < length defaultFruits = defaultFruits !! n
  | otherwise = last defaultFruits 

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

defaultFruits :: [PointItem]
defaultFruits =
  [ Fruit (0, 0) 100 Cherry,
    Fruit (0, 0) 300 Strawberry,
    Fruit (0, 0) 500 Orange,
    Fruit (0, 0) 700 Apple,
    Fruit (0, 0) 1000 Melon,
    Fruit (0, 0) 2000 Galaxian,
    Fruit (0, 0) 3000 Bell,
    Fruit (0, 0) 5000 Key
  ]

module Model.Item(
  PointItem(), 
  FruitType, defaultFruits
  Positioned, Position
) where

-- | Position of something in the level 
type Position = (Float, Float)

data Positioned a = Positioned a Position

-- | Score points
type Points = Int

-- | Item representing an object that can give point score to the player when eaten
-- | Dot is the most common item to be consumed and are needed to complete a level
-- | PowerPellet is a special item that gives the player the ability to eat ghosts
-- | Fruit is a special item that gives the player a large amount of points
data PointItem = 
    Dot {points :: Points} 
  | PowerPellet {points :: Points} 
  | Fruit {
    itemType :: FruitType,
    points :: Points
    } 
  deriving (Eq, Show)

-- | Types of fruit
data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key deriving (HasPoint,Eq, Show)

defaultFruits :: [Fruit]
defaultFruits = [
  Fruit Cherry 100,
  Fruit Strawberry 300,
  Fruit Orange 500,
  Fruit Apple 700,
  Fruit Melon 1000,
  Fruit Galaxian 2000,
  Fruit Bell 3000,
  Fruit Key 5000
  ]

module Model.Level(
  LevelNumber, mkLevelNumber,
  Tile(),
  DoorState(),
  LevelLayout(),
  Position(),
  PelletType(),
  Pellet(),
  Level, mkLevel,
  LevelSize, levelSize
) where

import Model.Characters(Ghost)

-- | Number/id of the level
type LevelNumber = Int

-- | Safe constructor for level number
mkLevelNumber :: Int -> Maybe LevelNumber
mkLevelNumber num
 | num >= 0 = Just num
 | otherwise = Nothing

-- | State of the ghost door
data DoorState = Open | Closed deriving (Eq, Show)

-- | Different types of tiles a level can have
-- | Wall is a tile player nor ghost can move through
-- | Floor is a tile player and ghost can move through 
-- | Door is a tile ghost can move through, but player can't, given that the doors are open
data Tile = Wall | Floor | GhostDoor DoorState deriving (Eq, Show)

-- | Level layout as a 2D Tile matrix
-- | The layout defines the floors, walls and doors of the level
type LevelLayout = [[Tile]]

-- | Position in the level
type Position = (Int, Int)

-- | Type of pallet
-- | Normal pellet is a pellet that gives points
-- | Power pellet is a pellet that makes ghosts vulnerable
data PelletType = Normal | Power deriving (Eq, Show)
-- Optional extention of power pellet types
-- data PalletType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key deriving (Eq, Show)

-- | Pellet in the level
data Pellet = Pellet {
    position    :: Position,
    pelletType  :: PelletType
}

-- | Level data
data Level = Level {
    levelNumber :: LevelNumber,
    name        :: String,
    pellets     :: [Pellet],
    enemies     :: [Ghost],
    layout      :: LevelLayout
}

-- | Size of the level layout in tiles (or Units so to speak)
type LevelSize = (Int, Int)

-- | Returns the size of the level (based on the level layout)
levelSize :: Level -> LevelSize
levelSize level = (x, y)
  where 
    x = length . layout $ level
    y = length . head . layout $ level

-- | Safe constructor for level
mkLevel :: LevelNumber -> String -> LevelLayout -> [Pellet] -> [Ghost] -> Maybe Level
mkLevel n name layout pellets enemies
  | validLayout layout = Just Level {
    levelNumber = n,
    name = name,
    pellets = pellets,
    enemies = enemies,
    layout = layout
    }
  | otherwise = Nothing

-- | Checks if the layout is valid
validLayout :: LevelLayout -> Bool
validLayout layout = undefined
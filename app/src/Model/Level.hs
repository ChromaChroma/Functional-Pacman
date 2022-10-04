module Model.Level(
  LevelNumber, mkLevelNumber,
  Tile(..), tileAt,
  DoorState(..),
  LevelLayout(..), layoutSize,
  Level(..), mkLevel, defaultLevel,
  LevelSize
) where

import Model.Characters(Ghost, blinky, pinky, inky, clyde, Player, defaultPlayer)
import Model.Items(PointItem(..), PointItem, defaultFruits)

-- | Number/id of the level
type LevelNumber = Int

-- | Safe constructor for level number
mkLevelNumber :: Int -> Maybe LevelNumber
mkLevelNumber num
 | num >= 0 = Just num
 | otherwise = Nothing

-- | Different types of tiles a level can have
-- | Wall is a tile player nor ghost can move through
-- | Floor is a tile player and ghost can move through 
-- | Door is a tile ghost can move through, but player can't, given that the doors are open
data Tile = Wall | Floor | GhostDoor DoorState deriving (Eq)

-- | State of the ghost door
data DoorState = Open | Closed deriving (Eq, Show)

-- | Level layout as a 2D Tile matrix
-- | The layout defines the floors, walls and doors of the level
type LevelLayout = [[Tile]]

-- | Level data
data Level = Level {
    levelNumber :: LevelNumber,
    items       :: [PointItem],
    layout      :: LevelLayout
}

-- | Size of the level layout in tiles (or Units so to speak)
type LevelSize = (Int, Int)

-- | Returns the size of the level (based on the level layout)
layoutSize :: LevelLayout -> LevelSize
layoutSize layout = (x, y)
  where
    x = length layout
    y = length . head $ layout

validLayout :: LevelLayout -> Bool
validLayout layout = length layout == x && all ((== y) . length) layout
  where
    (x, y) = layoutSize layout

-- | Safe constructor for level
mkLevel :: LevelNumber -> LevelLayout -> [PointItem] -> [Ghost] -> Player -> Maybe Level
mkLevel n layout items enemies player
  | validLayout layout = Just Level {
    levelNumber = n,
    items = items,
    -- enemies = enemies,
    -- player = player,
    -- items = defaultFruits,
    layout = layout
    }
  | otherwise = Nothing

tileAt :: Level -> (Int, Int) -> Tile
tileAt level (x, y) = layout level !! y!! x

-- | Default PacMan Maze level
defaultLevel :: Level
defaultLevel = Level {
  levelNumber = 0,
  items = [Dot (1,1) 10],
  layout = defaultLayout
  }

-- | Default PacMan level layout
-- defaultLayout :: LevelLayout
-- defaultLayout = [
--   [Wall, Wall,  Wall,   Wall,   Wall, Wall, Wall, Wall, Wall, Wall],
--   [Wall, Floor, Floor,  Floor,  Floor, Floor, Floor, Floor, Floor, Wall],
--   [Wall, Floor, Wall,   Wall,  Wall, Wall, Floor, Wall, Floor, Wall],
--   [Wall, Floor, Floor,  Floor,  Floor, Floor, Floor, Floor, Floor, Wall],
--   [Wall, Floor, Wall,   Floor,  Wall, Wall, Wall, Wall, Floor, Wall],
--   [Wall, Floor, Floor,  Floor,  Wall, Floor, Floor, Wall, Floor, Wall],
--   [Wall, Floor, Wall,   Floor,  Wall, Floor, Floor, Wall, Floor, Wall],
--   [Wall, Floor, Wall,   Floor,  Wall, Wall, GhostDoor Closed, Wall, Floor, Wall],
--   [Wall, Floor, Floor,  Floor,  Floor, Floor, Floor, Floor, Floor, Wall],
--   [Wall, Wall,  Wall,   Wall,   Wall, Wall, Wall, Wall, Wall, Wall]
--   ]


-- | Standard pacman level
standardLevel :: Level
standardLevel = Level {
  levelNumber = 0,
  items = [Dot (1,1) 10],
  layout = defaultLayout
  }

-- | Standard PacMan level layout (28 x 32)
defaultLayout :: LevelLayout
defaultLayout = [
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall],
  [Floor, Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor],
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Wall,   Wall,   Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Floor,  Wall],
  [Wall,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Floor,  Wall],
  [Wall,  Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall,   Wall]
  ]
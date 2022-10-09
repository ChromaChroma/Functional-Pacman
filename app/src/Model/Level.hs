module Model.Level(
  LevelNumber, mkLevelNumber,
  Tile(..), tileAt,
  DoorState(..),
  Layout(..), layoutSize,
  Level(..), mkLevel, defaultLevel,
  LevelSize
) where

import Model.Ghosts(Ghost, blinky, pinky, inky, clyde)
import Model.Player(Player, defaultPlayer)
import Model.Items(PointItem(..), PointItem, defaultFruits, Position)
import qualified Data.Maybe
import Data.Maybe ( fromJust )
import Model.Movement ()

-- | Number or id of the level
type LevelNumber = Int

-- | Different types of tiles a level can have
-- | Wall is a tile player nor ghost can move through
-- | Floor is a tile player and ghost can move through 
-- | Door is a tile ghost can move through, but player can't, given that the doors are open
data Tile = Wall | Floor | GhostDoor DoorState deriving (Eq)

-- | State of the ghost door
data DoorState = Open | Closed deriving (Eq, Show)

-- | Layout as a Tile matrix made from 2D lists
-- | The layout defines the floors, walls and doors of the level
type Layout = [[Tile]]

-- | Spawn location of the player
type PlayerSpawn = (Float, Float)

-- | Level data
data Level = Level {
    levelNumber :: LevelNumber,
    items       :: [PointItem],
    layout      :: Layout,
    playerSpawn :: PlayerSpawn
}

-- | Size of the level layout in amount of tiles
type LevelSize = (Int, Int)

-- | Safe constructor for level number
mkLevelNumber :: Int -> Maybe LevelNumber
mkLevelNumber num
 | num >= 0 = Just num
 | otherwise = Nothing

-- | Returns the size of the provided level layout
layoutSize :: Layout -> LevelSize
layoutSize layout = (length layout, length . head $ layout)

-- | Validates the size of the provided layout, checking that all lists are the correct length
validLayout :: Layout -> Bool
validLayout l = let (x, y) = layoutSize l in length l == y && all ((== x) . length) l
    
-- | Safe constructor for level
mkLevel :: LevelNumber 
  -> Layout 
  -> [PointItem] 
  -> [Ghost] 
  -> PlayerSpawn
  -> Maybe Level
mkLevel n layout items enemies spawn
  | validLayout layout = Just Level {
    levelNumber = n,
    items = items,
    layout = layout,
    playerSpawn = spawn
    }
  | otherwise = Nothing

-- | Gets the tile at the provided position in the layout if present, otherwise returns Nothing
tileAt :: Level -> (Int, Int) -> Maybe Tile
tileAt level (x, y)
 | x < lvlWidth || y < lvlHeight = Just $ lvlLayout !! y!! x
 | otherwise = Nothing
 where
  lvlLayout = layout level
  (lvlWidth, lvlHeight) = layoutSize lvlLayout

-- | Wrapper function for tileAt that wraps the index around if it is out of level size bounds
tileAtW :: Level -> (Int, Int) -> Tile
tileAtW level (x, y)
  | x > x' = tileAtW level (x - x', y)
  | y > y' = tileAtW level (x, y - y')
  | x < 0 = tileAtW level (x + x', y)
  | y < 0 = tileAtW level (x, y + y')
  | otherwise = fromJust $ tileAt level (x, y)
  where
    (x', y') = layoutSize $ layout level

-- |
-- | Default level
-- |

-- | Default PacMan level
defaultLevel :: Level
defaultLevel = Level {
  levelNumber = 0,
  items = defaultDots,
  layout = defaultLayout,
  playerSpawn = (14, 23)
  }

-- | Original PacMan layout (28 x 32 maze)
defaultLayout :: Layout
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
  [Floor, Floor,  Floor,  Floor,  Floor,  Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Wall,   Wall,   GhostDoor Open,   GhostDoor Open,   Wall,   Wall,   Wall,   Floor,  Wall,   Wall,   Floor,  Wall,   Floor,  Floor,  Floor,  Floor,  Floor],
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
  
defaultDots :: [PointItem]
defaultDots = [Dot (1,1) 10]

-- | Development level layout
-- defaultLayout :: Layout
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
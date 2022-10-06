module Model.Level(
  LevelNumber, mkLevelNumber,
  Tile(..), tileAt,
  DoorState(..),
  LevelLayout(..), layoutSize,
  Level(..), mkLevel, defaultLevel,
  LevelSize
) where

import Model.Characters(Ghost, blinky, pinky, inky, clyde, Player, defaultPlayer, Movable (getPosition))
import Model.Items(PointItem(..), PointItem, defaultFruits, Position)
import qualified Data.Maybe
import Data.Maybe
import Model.Movement
import Model.Game

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

-- moveToPosition :: Movable a => Level -> a -> Position -> a
-- moveToPosition level m (x, y) = 

isValidPosition :: Movable a => (Tile -> Bool) -> Level -> a -> Bool
isValidPosition p level m = p . tileAtW level . intPosition $ getPosition m

isValidPlayerPosition :: Level -> Player -> Bool
isValidPlayerPosition = isValidPosition isValid
  where
    isValid Wall = False
    isValid (GhostDoor _) = False
    isValid _ = True

isValidGhostPosition :: Level -> Ghost -> Bool
isValidGhostPosition = isValidPosition isValid
  where
    isValid Wall = False
    isValid (GhostDoor Open) = False
    isValid (GhostDoor Closed) = True
    isValid _ = True



-- check bufdirection mvoe
-- check direction move
-- stay sameplace, update dir to stop

makeMove :: Movable a => GameState -> a -> a
makeMove gs m
  | isValidPlayerPosition (level gs) normalMove = normalMove
  | otherwise = a
  where
    (mx, my) = getPosition a -- get current x,y
    -- check if player is on axis of direction (Up/down = x is whole number, Left/right = y is wholenumber) before checking if can move direction
    -- This kind of checks if player position is on crossroads
    
    -- if so (guard cases) check if move in direction is valid like in engine
    -- if so make move (return new player) else return original player (movable)
    -- normalMove is 
    normalMove = moveFull m $ direction gs
  
moveFull :: Movable a => a -> Direction -> a
moveFull m dir = setPosition m (moveFullUnit m dir)
  where
    moveFullUnit m dir = case dir of
      Up -> (x, y -1)
      Down -> (x, y + 1)
      Left -> (x -1, y)
      Right -> (x + 1, y)
      _ -> (x, y)
      where
        (x, y) = getPosition m
   -- Player with new (x.y) position
validateMove = undefined




-- moveToPosition :: Movable a => Level -> a -> Maybe a
-- moveToPosition level movable =
--   where
--     (x, y) = getPosition movable
--     tile = case tileAt level (x, y) of
--       Just t -> t
--       Nothing -> -- do wrap check

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
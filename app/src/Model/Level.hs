module Model.Level
  ( LevelNumber,
    mkLevelNumber,
    Tile (..),
    tileAtW,
    DoorState (..),
    Layout (..),
    layoutSize,
    Level (..),
    mkLevel,
    defaultLevel,
    LevelSize,
    levelIntersections,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Maybe
import Model.Ghosts (Ghost, blinky, clyde, inky, pinky)
import Model.Items (PointItem (..), Position, defaultFruits, mkDot, mkPowerPellet)
import Model.Movement ()
import Model.Player (Player, defaultPlayer)

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
-- | The layout defines the floors and walls of the level
-- | The level's first tile (index 0 0) is the bottom left of a level
type Layout = [[Tile]]

-- | Spawn location of the player
type PlayerSpawn = (Float, Float)

-- | Level data
data Level = Level
  { levelNumber :: LevelNumber,
    items :: [PointItem],
    layout :: Layout,
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
layoutSize layout = (length . head $ layout, length layout)

-- | Validates the size of the provided layout, checking that all lists are the correct length
validLayout :: Layout -> Bool
validLayout l = let (x, y) = layoutSize l in length l == y && all ((== x) . length) l

-- | Safe constructor for level
mkLevel ::
  LevelNumber ->
  Layout ->
  [PointItem] ->
  [Ghost] ->
  PlayerSpawn ->
  Maybe Level
mkLevel n layout items enemies spawn
  | validLayout layout =
    Just
      Level
        { levelNumber = n,
          items = items,
          layout = layout,
          playerSpawn = spawn
        }
  | otherwise = Nothing

-- | Gets the tile at the provided position in the layout if present, otherwise returns Nothing
tileAt :: Level -> (Int, Int) -> Maybe Tile
tileAt level (x, y)
  | x < lvlWidth && y < lvlHeight = Just $ lvlLayout !! y !! x
  | otherwise = Nothing
  where
    lvlLayout = layout level
    (lvlWidth, lvlHeight) = layoutSize lvlLayout

-- | Function to find tile on coordinate in level, wrapping around if out of bounds
tileAtW :: Level -> (Int, Int) -> Tile
tileAtW level (x, y)
  | x < 0 = tileAtW level (x + x', y)
  | y < 0 = tileAtW level (x, y + y')
  | otherwise = fromJust $ tileAt level (x `mod` x', y `mod` y')
  where
    (x', y') = layoutSize $ layout level

-- | an Intersecion is an (Int, Int) tuple corresponding with a tile position on which 3 or more directions can be moved
type Intersection = (Int, Int)

levelIntersections :: Level -> [Intersection]
levelIntersections level =
  [ (x, y)
    | x <- [0 .. (width -1)],
      y <- [0 .. (height -1)],
      tileAtW level (x, y) == Floor && isIntersection (x, y)
  ]
  where
    (width, height) = layoutSize $ layout level
    isIntersection (x', y') = (length . filter (== Floor) $ [left, right, up, down]) >= 3
      where
        left = tileAtW level (x' - 1, y')
        right = tileAtW level (x' + 1, y')
        up = tileAtW level (x', y' + 1)
        down = tileAtW level (x', y' - 1)

-- |
-- | Default level
-- |

-- | Default PacMan level
defaultLevel :: Level
defaultLevel =
  Level
    { levelNumber = 0,
      items = defaultDots ++ defaultPowerPellets,
      layout = defaultLayout,
      playerSpawn = (14, 7)
    }

-- | Original PacMan layout (28 x 32 maze)
defaultLayout :: Layout
defaultLayout =
  [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall],
    [Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall],
    [Wall, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall],
    [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
    [Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Floor, Floor, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Floor, Floor, Floor, Floor, Floor, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Floor, Floor, Floor, Floor, Floor],
    [Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Wall, Floor, Wall, Wall, Wall, Wall, Floor, Wall],
    [Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall, Wall, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  ]

defaultDots :: [PointItem]
defaultDots =
  [ mkDot (1, 1),
    mkDot (2, 1),
    mkDot (3, 1),
    mkDot (4, 1),
    mkDot (5, 1),
    mkDot (6, 1),
    mkDot (7, 1),
    mkDot (8, 1),
    mkDot (9, 1),
    mkDot (10, 1),
    mkDot (11, 1),
    mkDot (12, 1),
    mkDot (13, 1),
    mkDot (14, 1),
    mkDot (15, 1),
    mkDot (16, 1),
    mkDot (17, 1),
    mkDot (18, 1),
    mkDot (19, 1),
    mkDot (20, 1),
    mkDot (21, 1),
    mkDot (22, 1),
    mkDot (23, 1),
    mkDot (24, 1),
    mkDot (25, 1),
    mkDot (26, 1),
    mkDot (1, 2),
    mkDot (12, 2),
    mkDot (15, 2),
    mkDot (26, 2),
    mkDot (1, 3),
    mkDot (12, 3),
    mkDot (15, 3),
    mkDot (26, 3),
    mkDot (1, 4),
    mkDot (2, 4),
    mkDot (3, 4),
    mkDot (4, 4),
    mkDot (5, 4),
    mkDot (6, 4),
    mkDot (9, 4),
    mkDot (10, 4),
    mkDot (11, 4),
    mkDot (12, 4),
    mkDot (15, 4),
    mkDot (16, 4),
    mkDot (17, 4),
    mkDot (18, 4),
    mkDot (21, 4),
    mkDot (22, 4),
    mkDot (23, 4),
    mkDot (24, 4),
    mkDot (25, 4),
    mkDot (26, 4),
    mkDot (3, 5),
    mkDot (6, 5),
    mkDot (9, 5),
    mkDot (18, 5),
    mkDot (21, 5),
    mkDot (24, 5),
    mkDot (3, 6),
    mkDot (6, 6),
    mkDot (9, 6),
    mkDot (18, 6),
    mkDot (21, 6),
    mkDot (24, 6),
    mkDot (2, 7),
    mkDot (3, 7),
    mkDot (6, 7),
    mkDot (7, 7),
    mkDot (8, 7),
    mkDot (9, 7),
    mkDot (10, 7),
    mkDot (11, 7),
    mkDot (12, 7),
    mkDot (13, 7),
    mkDot (14, 7),
    mkDot (15, 7),
    mkDot (16, 7),
    mkDot (17, 7),
    mkDot (18, 7),
    mkDot (19, 7),
    mkDot (20, 7),
    mkDot (21, 7),
    mkDot (24, 7),
    mkDot (25, 7),
    mkDot (1, 8),
    mkDot (6, 8),
    mkDot (12, 8),
    mkDot (15, 8),
    mkDot (21, 8),
    mkDot (26, 8),
    mkDot (1, 9),
    mkDot (6, 9),
    mkDot (12, 9),
    mkDot (15, 9),
    mkDot (21, 9),
    mkDot (26, 9),
    mkDot (1, 10),
    mkDot (2, 10),
    mkDot (3, 10),
    mkDot (4, 10),
    mkDot (5, 10),
    mkDot (6, 10),
    mkDot (7, 10),
    mkDot (8, 10),
    mkDot (9, 10),
    mkDot (10, 10),
    mkDot (11, 10),
    mkDot (12, 10),
    mkDot (15, 10),
    mkDot (16, 10),
    mkDot (17, 10),
    mkDot (18, 10),
    mkDot (19, 10),
    mkDot (20, 10),
    mkDot (21, 10),
    mkDot (22, 10),
    mkDot (23, 10),
    mkDot (24, 10),
    mkDot (25, 10),
    mkDot (26, 10),
    mkDot (6, 11),
    mkDot (21, 11),
    mkDot (6, 12),
    mkDot (21, 12),
    mkDot (6, 13),
    mkDot (21, 13),
    mkDot (6, 14),
    mkDot (21, 14),
    mkDot (6, 15),
    mkDot (21, 15),
    mkDot (6, 16),
    mkDot (21, 16),
    mkDot (6, 17),
    mkDot (21, 17),
    mkDot (6, 18),
    mkDot (21, 18),
    mkDot (6, 19),
    mkDot (21, 19),
    mkDot (6, 20),
    mkDot (21, 20),
    mkDot (6, 21),
    mkDot (21, 21),
    mkDot (1, 22),
    mkDot (2, 22),
    mkDot (3, 22),
    mkDot (4, 22),
    mkDot (5, 22),
    mkDot (6, 22),
    mkDot (9, 22),
    mkDot (10, 22),
    mkDot (11, 22),
    mkDot (12, 22),
    mkDot (15, 22),
    mkDot (16, 22),
    mkDot (17, 22),
    mkDot (18, 22),
    mkDot (21, 22),
    mkDot (22, 22),
    mkDot (23, 22),
    mkDot (24, 22),
    mkDot (25, 22),
    mkDot (26, 22),
    mkDot (1, 23),
    mkDot (6, 23),
    mkDot (9, 23),
    mkDot (18, 23),
    mkDot (21, 23),
    mkDot (26, 23),
    mkDot (1, 24),
    mkDot (6, 24),
    mkDot (9, 24),
    mkDot (18, 24),
    mkDot (21, 24),
    mkDot (26, 24),
    mkDot (1, 25),
    mkDot (2, 25),
    mkDot (3, 25),
    mkDot (4, 25),
    mkDot (5, 25),
    mkDot (6, 25),
    mkDot (7, 25),
    mkDot (8, 25),
    mkDot (9, 25),
    mkDot (10, 25),
    mkDot (11, 25),
    mkDot (12, 25),
    mkDot (13, 25),
    mkDot (14, 25),
    mkDot (15, 25),
    mkDot (16, 25),
    mkDot (17, 25),
    mkDot (18, 25),
    mkDot (19, 25),
    mkDot (20, 25),
    mkDot (21, 25),
    mkDot (22, 25),
    mkDot (23, 25),
    mkDot (24, 25),
    mkDot (25, 25),
    mkDot (26, 25),
    mkDot (1, 26),
    mkDot (6, 26),
    mkDot (12, 26),
    mkDot (15, 26),
    mkDot (21, 26),
    mkDot (26, 26),
    mkDot (6, 27),
    mkDot (12, 27),
    mkDot (15, 27),
    mkDot (21, 27),
    mkDot (1, 28),
    mkDot (6, 28),
    mkDot (12, 28),
    mkDot (15, 28),
    mkDot (21, 28),
    mkDot (26, 28),
    mkDot (1, 29),
    mkDot (2, 29),
    mkDot (3, 29),
    mkDot (4, 29),
    mkDot (5, 29),
    mkDot (6, 29),
    mkDot (7, 29),
    mkDot (8, 29),
    mkDot (9, 29),
    mkDot (10, 29),
    mkDot (11, 29),
    mkDot (12, 29),
    mkDot (15, 29),
    mkDot (16, 29),
    mkDot (17, 29),
    mkDot (18, 29),
    mkDot (19, 29),
    mkDot (20, 29),
    mkDot (21, 29),
    mkDot (22, 29),
    mkDot (23, 29),
    mkDot (24, 29),
    mkDot (25, 29),
    mkDot (26, 29)
  ]

defaultPowerPellets :: [PointItem]
defaultPowerPellets =
  [ mkPowerPellet (1, 7),
    mkPowerPellet (26, 7),
    mkPowerPellet (1, 27),
    mkPowerPellet (26, 27)
  ]

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
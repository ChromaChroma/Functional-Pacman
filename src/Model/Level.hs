module Model.Level
  ( LevelNumber,
    mkLevelNumber,
    Tile (..),
    tileAtW,
    Layout (..),
    layoutSize,
    Level (..),
    mkLevel,
    isLevelComplete,
    LevelSize,
    levelIntersections,
    levelFloorSplits,
    rotR,
    rotL,
    mirrorH,
    mirrorV,
  )
where

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Maybe
import Model.Items (PointItem (..), Position, mkDot, mkPowerPellet)
import Model.Movement ()
import Model.Player (Player)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Number or id of the level
type LevelNumber = Int

-- | Different types of tiles a level can have
-- | Wall is a tile player nor ghost can move through
-- | Floor is a tile player and ghost can move through
-- | Door is a tile ghost can move through, but player can't
data Tile = Wall | Floor | GhostDoor  deriving (Eq)


-- | Layout as a Tile matrix made from 2D lists
-- | The layout defines the floors and walls of the level
-- | The level's first tile (index 0 0) is the bottom left of a level
newtype Layout a = Layout [[a]] deriving (Eq)

-- | Spawn location of the player
type PlayerSpawn = (Float, Float)

-- | Level data
data Level = Level
  { levelNumber :: LevelNumber,
    items :: [PointItem],
    layout :: Layout Tile,
    playerSpawn :: PlayerSpawn
  }
  deriving (Eq)

-- | Size of the level layout in amount of tiles
type LevelSize = (Int, Int)

-- | an Intersecion is an (Int, Int) tuple corresponding with a tile position on which 3 or more directions can be moved
type Intersection = (Int, Int)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Functor Layout where
  fmap f (Layout xss) = Layout (map (map f) xss)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | Rotate a kayout 90 degrees clockwise
rotR :: Layout a -> Layout a
rotR (Layout xss) = Layout (reverse $ transpose xss)

-- | Rotate a layout 90 degrees counter-clockwise
rotL :: Layout a -> Layout a
rotL (Layout xss) = Layout (transpose $ reverse xss)

-- | Mirror a layout horizontally
mirrorH :: Layout a -> Layout a
mirrorH (Layout xss) = Layout (map reverse xss)

-- | Mirror a layout vertically
mirrorV :: Layout a -> Layout a
mirrorV (Layout xss) = Layout (reverse xss)

-- | Safe constructor for level number
mkLevelNumber :: Int -> Maybe LevelNumber
mkLevelNumber num
  | num >= 0 = Just num
  | otherwise = Nothing

-- | Safe constructor for level
mkLevel ::
  LevelNumber ->
  Layout Tile ->
  [PointItem] ->
  PlayerSpawn ->
  Maybe Level
mkLevel n layout items spawn
  | isValidLayout layout =
    Just
      Level
        { levelNumber = n,
          layout = layout,
          items = items,
          playerSpawn = spawn
        }
  | otherwise = Nothing

-- | Validates the size of the provided layout, checking that all lists are the correct length
isValidLayout :: Layout a -> Bool
isValidLayout tl@(Layout xss) = let (x, y) = layoutSize tl in length xss == y && all ((== x) . length) xss

-- | Returns the size of the provided level layout
layoutSize :: Layout a -> LevelSize
layoutSize (Layout xss) = (length . head $ xss, length xss)

-- | Validates if the level is complete, meaning all dots are eaten
isLevelComplete :: Level -> Bool
isLevelComplete level = not $ any isDot (items level)
  where
    isDot (Dot _ _) = True
    isDot _ = False

-- | Function to find tile on coordinate in level, wrapping around if out of bounds
tileAtW :: Level -> (Int, Int) -> Tile
tileAtW level@Level {layout = (Layout xss)} (x, y)
  | x < 0 = tileAtW level (x + x', y)
  | y < 0 = tileAtW level (x, y + y')
  | x > x' = tileAtW level (x - x', y)
  | y > y' = tileAtW level (x, y - y')
  | otherwise = xss !! y !! x
  where
    (w, h) = layoutSize . layout $ level
    (x', y') = (w -1, h -1)

filterFloorOnDirections :: Level -> Int -> [(Int, Int)]
filterFloorOnDirections _ 0 = []
filterFloorOnDirections level i =
  [ (x, y)
    | x <- [0 .. (width -1)],
      y <- [0 .. (height -1)],
      tileAtW level (x, y) == Floor,
      isIntersection (x, y)
  ]
  where
    (width, height) = layoutSize $ layout level
    isIntersection coords = (length . filter (== Floor) $ neighbors level coords) == i

-- | Calculate the neighboring tiles of a given tile position
neighbors :: Level -> (Int, Int) -> [Tile]
neighbors level (x, y) = [left, right, up, down]
  where
    (width, height) = layoutSize $ layout level
    left = tileAtW level (x - 1, y)
    right = tileAtW level (x + 1, y)
    up = tileAtW level (x, y + 1)
    down = tileAtW level (x, y - 1)

-- Calculates the floor intersections of the level
levelIntersections :: Level -> [Intersection]
levelIntersections level = filterFloorOnDirections level 3 ++ filterFloorOnDirections level 4

-- Calculates the floor deadends of the level
levelDeadEnds :: Level -> [Intersection]
levelDeadEnds level = filterFloorOnDirections level 1

-- -- Calculates the floor corners of the level
-- levelCorners :: Level -> [Intersection]
-- levelCorners level = filterFloorOnDirections level 2

-- Calculates the floor intersections of the level
levelCorners :: Level -> [Intersection]
levelCorners level =
  [ (x, y)
    | x <- [0 .. (width -1)],
      y <- [0 .. (height -1)],
      tileAtW level (x, y) == Floor,
      isCorner (x, y)
  ]
  where
    (width, height) = layoutSize $ layout level
    -- isIntersection (x', y') = (length . filter (== Floor) $ [left, right, up, down]) >= 3
    isCorner (x', y') = left && up || left && down || right && up || right && down
      where
        left = tileAtW level (x' - 1, y') == Floor
        right = tileAtW level (x' + 1, y') == Floor
        up = tileAtW level (x', y' + 1) == Floor
        down = tileAtW level (x', y' - 1) == Floor

-- Calculates the floor path splits of the level
levelFloorSplits :: Level -> [Intersection]
levelFloorSplits level = levelIntersections level ++ levelCorners level ++ levelDeadEnds level

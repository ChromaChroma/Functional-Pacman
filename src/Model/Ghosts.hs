module Model.Ghosts
  ( Ghost (..),
    Name (..),
    EatenState (..),
    Direction (..),
    blinky,
    pinky,
    inky,
    clyde,
    isEaten,
    isNotEaten,
    collidesWithMovable,
    opp,
    turnGhostsAround,
    startFrightened,
    -- slowGhostsDown,
    speedGhostsUp,
    defaultGhosts,
    slowGhostDownTunnel,
    speedGhostUpTunnel,
    posToTile,
    ghostTilePosition
  )
where

import Model.Movement
import Prelude hiding (Left, Right, Down, Up)
-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | State of living of a Ghost
data EatenState = NotEaten | Eaten deriving (Eq, Show)

-- | Name of a Ghost
data Name = Blinky | Pinky | Inky | Clyde deriving (Eq, Show, Enum)

-- | The ghost's current state
data Ghost = Ghost
  { name :: Name,
    position :: Position,
    speed :: Speed,
    eatenState :: EatenState,
    direction :: Direction,
    opDirection :: Direction,
    nextDirection :: Direction,
    isInTunnel :: Bool,
    goesBack :: Bool,
    wellPositioned :: Bool,
    wellPositionedTarget :: Bool,
    choseRandom :: Bool
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Positioned Ghost where
  getPosition = position
  setPosition ghost pos = ghost {position = pos}

instance Collidable Ghost

instance Movable Ghost where
  getSpeed = speed

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

isEaten :: Ghost -> Bool
isEaten = (== Eaten) . eatenState

isNotEaten :: Ghost -> Bool
isNotEaten = not . isEaten

collidesWithMovable :: (Movable a, Collidable a) => Ghost -> a -> Bool
collidesWithMovable ghost m = isNotEaten ghost && ghost `collides` m


--Ghosts reverse direction both in scatter and chasing mode:

--When changing to and from scatter mode:
turnGhostsAround :: [Ghost] -> [Ghost]
turnGhostsAround ghosts = map turn1GhostAround ghosts where
  turn1GhostAround ghost = case isEaten ghost of
    False -> ghost {direction = opp (direction ghost), opDirection = opp (opDirection ghost), nextDirection = opp (nextDirection ghost)}
    True -> ghost --isEaten state: ghost doesn't turn around if eaten

--This one is for frightened mode
startFrightened :: [Ghost] -> [Ghost]
startFrightened ghosts = map start1Frightened ghosts where
  start1Frightened ghost = case isEaten ghost of
    False -> case isInTunnel ghost of
      False -> ghost {speed = 0.08, direction = opp (direction ghost), opDirection = opp (opDirection ghost), nextDirection = opp (nextDirection ghost)} --position = gTilePos,
      True -> ghost {speed = 0.04, direction = opp (direction ghost), opDirection = opp (opDirection ghost)} --position = gTilePos,
    True -> ghost --isEaten state: ghost doesn't turn around & slow down if eaten
    where
      gTilePos = ghostTilePosition ghost

-- slowGhostsDown :: [Ghost] -> [Ghost]
-- slowGhostsDown ghosts = map slow1GhostDown ghosts where
--   slow1GhostDown ghost = ghost {speed = 0.08}

speedGhostsUp :: [Ghost] -> [Ghost]
speedGhostsUp ghosts = map speed1GhostUp ghosts where
  speed1GhostUp ghost = case isEaten ghost of
    False -> case isInTunnel ghost of
      False -> ghost {speed = 0.125} --position = gTilePos,
      True  -> ghost {speed = 0.125/2} --position = gTilePos,
    True  -> ghost
    where
      gTilePos = ghostTilePosition ghost

slowGhostDownTunnel :: Ghost -> Ghost
slowGhostDownTunnel ghost = ghost {speed = 1/2 * (speed ghost)}
  where
    gTilePos = ghostTilePosition ghost

speedGhostUpTunnel :: Ghost -> Ghost
speedGhostUpTunnel ghost = ghost {speed = 2 * (speed ghost)}
  where
    gTilePos = ghostTilePosition ghost


--------------------------------------------------------------------------------
--Convert float coordinate to tile (int) coordinate
posToTile :: Position -> (Int, Int)
posToTile (x, y) = (round x, round y)

ghostTilePosition :: Ghost -> Position
ghostTilePosition gh = gT
  where
    gT = (fromIntegral gX, fromIntegral gY)
    (gX, gY) = posToTile (getPosition gh)

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

-- | Default ghost constructors for each original ghost
blinky :: Ghost
blinky = Ghost Blinky (12, 16) 0.125 NotEaten Up Stop Right False False False False False--speed is 75%, player's is 80% (0.125)

pinky :: Ghost
pinky = Ghost Pinky (13, 16) 0.125 NotEaten Up Stop Left False False False False False

inky :: Ghost
inky = Ghost Inky (14, 16) 0.125 NotEaten Up Stop Left False False False False False

clyde :: Ghost
clyde = Ghost Clyde (15, 16) 0.125 NotEaten Up Stop Right False False False False False

defaultGhosts :: [Ghost]
defaultGhosts = [blinky, pinky, inky, clyde]

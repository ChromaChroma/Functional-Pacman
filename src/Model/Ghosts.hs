module Model.Ghosts
  ( Ghost (..),
    Name (..),
    EatenState (..),
    Direction (..),
    isEaten,
    isNotEaten,
    collidesWithMovable,
    opp,
    turnGhostsAround,
    frightenGhosts,
    -- slowGhostsDown,
    speedGhostsUp,
    slowGhostDownTunnel,
    speedGhostUpTunnel,
    ghostTilePosition,
    respawnGhosts,
    moveGhostsOutSpawn,
  )
where

import Model.Movement
import Prelude hiding (Down, Left, Right, Up)

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
    wellPositionedTarget :: Bool
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
turnGhostsAround = map turn1GhostAround
  where
    turn1GhostAround ghost
      | isEaten ghost = ghost
      | otherwise = ghost {direction = opp (direction ghost), opDirection = opp (opDirection ghost), nextDirection = opp (nextDirection ghost)} --isEaten state: ghost doesn't turn around if eaten

--This one is for frightened mode
frightenGhosts :: [Ghost] -> [Ghost]
frightenGhosts = map start1Frightened
  where
    start1Frightened ghost
      | isEaten ghost = ghost
      | isInTunnel ghost = ghost {speed = 0.04, direction = opp (direction ghost), opDirection = opp (opDirection ghost)} --position = gTilePos,
      | otherwise = ghost {speed = 0.08, direction = opp (direction ghost), opDirection = opp (opDirection ghost), nextDirection = opp (nextDirection ghost)} --position = gTilePos,

-- slowGhostsDown :: [Ghost] -> [Ghost]
-- slowGhostsDown = map slow1GhostDown 
--   where
--     slow1GhostDown ghost = ghost {speed = 0.08}

speedGhostsUp :: [Ghost] -> [Ghost]
speedGhostsUp = map speed1GhostUp
  where
    speed1GhostUp ghost
      | isEaten ghost = ghost
      | isInTunnel ghost = ghost {speed = 0.125 / 2}
      | otherwise = ghost {speed = 0.125}

slowGhostDownTunnel :: Ghost -> Ghost
slowGhostDownTunnel ghost
  | isEaten ghost = ghost
  | otherwise = ghost {speed = 0.5 * speed ghost}

speedGhostUpTunnel :: Ghost -> Ghost
speedGhostUpTunnel ghost
  | isEaten ghost = ghost
  | otherwise = ghost {speed = 2 * speed ghost}

--------------------------------------------------------------------------------
--If player is killed:
respawnGhosts :: [Ghost] -> [Ghost]
respawnGhosts = map respawnGhosts
  where
    respawnGhosts gh = case direction gh of
      Stop -> gh --ghost didn't go out of spawn yet
      _ -> case name gh of
        Blinky -> gh {position = (13.0, 19.0), direction = Right, opDirection = Left, nextDirection = Right}
        Pinky -> gh {position = (13.0, 19.0), direction = Left, opDirection = Right, nextDirection = Left}
        Inky -> gh {position = (14.0, 19.0), direction = Left, opDirection = Right, nextDirection = Left}
        Clyde -> gh {position = (14.0, 19.0), direction = Right, opDirection = Left, nextDirection = Right}

--------------------------------------------------------------------------------

moveGhostsOutSpawn :: Int -> [Ghost] -> [Ghost]
moveGhostsOutSpawn dots = map move1GhostOutSpawn
  where
    move1GhostOutSpawn gh = case direction gh of
      Stop -> case name gh of
        Blinky -> if dots <= 241 then moveUp else gh
        Pinky -> if dots <= 239 then moveUp else gh
        Inky -> if dots < 210 then moveUp else gh
        Clyde -> if dots < 150 then moveUp else gh
      _ -> gh
      where
        moveUp = gh {direction = Up, opDirection = Down}

--------------------------------------------------------------------------------
-- Helper Function
--------------------------------------------------------------------------------

ghostTilePosition :: Ghost -> Position
ghostTilePosition gh = (fromIntegral gX, fromIntegral gY)
  where
    (gX, gY) = intPosition (getPosition gh)

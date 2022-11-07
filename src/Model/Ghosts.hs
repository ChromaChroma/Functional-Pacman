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
    startFrightened,
    -- slowGhostsDown,
    speedGhostsUp,
    slowGhostDownTunnel,
    speedGhostUpTunnel,
    posToTile,
    ghostTilePosition,
    startGhostsAgain,
    moveGhostsOutSpawn
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
slowGhostDownTunnel ghost = case isEaten ghost of
  True  -> ghost
  False -> ghost {speed = 1/2 * (speed ghost)}
  where
    gTilePos = ghostTilePosition ghost

speedGhostUpTunnel :: Ghost -> Ghost
speedGhostUpTunnel ghost = case isEaten ghost of
  True  -> ghost
  False -> ghost {speed = 2 * (speed ghost)}
  where
    gTilePos = ghostTilePosition ghost

--------------------------------------------------------------------------------
--If player is killed:
startGhostsAgain :: [Ghost] -> [Ghost]
startGhostsAgain ghs = map start1GhostAgain ghs where
  start1GhostAgain gh = case direction gh of
                          Stop -> gh --ghost didn't go out of spawn yet
                          _    -> case name gh of
                                    Blinky -> gh {position = (13.0,19.0), direction = Right, opDirection = Left, nextDirection = Right}
                                    Pinky  -> gh {position = (13.0,19.0), direction = Left, opDirection = Right, nextDirection = Left}
                                    Inky   -> gh {position = (14.0,19.0), direction = Left, opDirection = Right, nextDirection = Left}
                                    Clyde  -> gh {position = (14.0,19.0), direction = Right, opDirection = Left, nextDirection = Right}

--------------------------------------------------------------------------------

moveGhostsOutSpawn :: Int -> [Ghost] -> [Ghost]
moveGhostsOutSpawn dots ghs = map move1GhostOutSpawn ghs where
  move1GhostOutSpawn gh = case direction gh of
    Stop -> case name gh of
      Blinky -> if dots <= 241 then moveUp else gh
      Pinky  -> if dots <= 239 then moveUp else gh
      Inky   -> if dots < 210 then moveUp else gh
      Clyde  -> if dots < 150 then moveUp else gh
    _    -> gh
    where
      moveUp = gh {direction = Up, opDirection = Down}



--------------------------------------------------------------------------------
--Convert float coordinate to tile (int) coordinate
posToTile :: Position -> (Int, Int)
posToTile (x, y) = (round x, round y)

ghostTilePosition :: Ghost -> Position
ghostTilePosition gh = gT
  where
    gT = (fromIntegral gX, fromIntegral gY)
    (gX, gY) = posToTile (getPosition gh)


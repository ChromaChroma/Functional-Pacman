module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    GhostMode (..),
    Time,
    tickDurationIn,
    checkCollisions,
    checkGameOver,
    frightenedDuration,
  )
where

import Model.Ghosts (Ghost (eatenState), EatenState (NotEaten, Eaten), blinky, clyde, collidesWithMovable, inky, pinky, isNotEaten)
import qualified Model.Items as I
import Model.Level (Level (items, playerSpawn), defaultLevel)
import Model.Movement (Collidable (collides), Movable (getSpeed), Positioned (setPosition))
import Model.Player (Player (lives), defaultPlayer, isAlive, position, rmLife)
import Model.Score (Points)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Time the game or the tickTimer has been running in milliseconds
type Time = Int

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | GameOver deriving (Eq, Show)

-- | Modes the ghosts can be in
-- | Chasing    : is the mode in which ghosts chase the player
-- | Frightened : is the mode in which ghosts run away from the player
-- | Scatter    : is the mode in which ghosts move to their specific location
data GhostMode = Chasing | Frightened | Scatter deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState
  { status :: Status,
    player :: Player,
    level :: Level,
    elapsedTime :: Time,
    tickTimer :: Time,
    ghosts :: [Ghost],
    points :: Points,
    frightenedTime :: Time,
    ghostMode :: GhostMode
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

loadGame :: Level -> [Ghost] -> Player -> GameState
loadGame lvl ghosts pl =
  GameState
    { status = Active,
      elapsedTime = 0,
      tickTimer = 0,
      player = pl,
      level = lvl,
      ghosts = ghosts,
      points = 0,
      frightenedTime = 0,
      ghostMode = Scatter
    }

-- | Check if game is over and update it if necessary
checkGameOver :: GameState -> GameState
checkGameOver gs
  | isAlive' = gs
  | otherwise = gs {status = GameOver}
  where
    isAlive' = isAlive . lives . player $ gs

-- | The specified minimal duration between each game tick
tickDurationIn :: Time
tickDurationIn = 30

-- | Logic for collisions
checkCollisions :: GameState -> GameState
checkCollisions = checkItemCollisions . checkGhostCollisions

checkItemCollisions :: GameState -> GameState
checkItemCollisions gs = foldr (\item -> removeItem item . addItemScore item . handleItemType item) gs (filter (player gs `collides`) (items . level $ gs))
  where
    removeItem item gs = gs {level = (level gs) {items = filter (/= item) (items . level $ gs)}}
    addItemScore item gs = gs {points = points gs + I.points item}
    handleItemType item gs = case item of
      I.PowerPellet _ _ -> gs {ghostMode = Frightened, frightenedTime = 0}
      _ -> gs

checkGhostCollisions :: GameState -> GameState
checkGhostCollisions gs = handleGhostCollisions gs (filter (`collidesWithMovable` player gs) $ ghosts gs)

handleGhostCollisions :: GameState -> [Ghost] -> GameState
handleGhostCollisions gs [] = gs
handleGhostCollisions gs (g : _) =
  if ghostMode gs == Frightened && isNotEaten g
    then eatGhost gs
    else respawnPlayer . reduceLife $ gs
  where
    eatGhost gs = gs {ghosts = map (\x -> if x == g then x {eatenState = Eaten} else x) (ghosts gs)}
    reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
    respawnPlayer gs
      | isAlive . lives $ player gs = gs {player = (player gs) {position = playerSpawn . level $ gs}}
      | otherwise = gs {status = GameOver}

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

frightenedDuration :: Time
frightenedDuration = 5000

defaultGame :: GameState
defaultGame = loadGame lvl ghosts pl
  where
    lvl = defaultLevel
    pl = setPosition defaultPlayer (playerSpawn lvl)
    ghosts = [blinky, pinky, inky, clyde]

module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    Time,
    tickDurationIn,
    checkCollisions
  )
where

import Model.Ghosts (Ghost, blinky, clyde, inky, pinky)
import qualified Model.Items as I
import Model.Level (Level (items, playerSpawn), defaultLevel)
import Model.Movement (Collidable (collides), Direction (..), Positioned (setPosition))
import Model.Player (Player (lives), defaultPlayer, isAlive, position, rmLife)
import Prelude hiding (Left, Right)

-- | Time the game or the tickTimer has been running in milliseconds
type Time = Int

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | Lost deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState
  { status :: Status,
    player :: Player,
    level :: Level,
    elapsedTime :: Time,
    tickTimer :: Time,
    direction :: Direction,
    bufDirection :: Direction,
    ghosts :: [Ghost]
  }

loadGame :: Level -> [Ghost] -> Player -> GameState
loadGame lvl ghosts pl =
  GameState
    { status = Active,
      elapsedTime = 0,
      tickTimer = 0,
      direction = Stop,
      bufDirection = Stop,
      player = pl,
      level = lvl,
      ghosts = ghosts
    }

defaultGame :: GameState
defaultGame = loadGame lvl ghosts pl
  where
    lvl = defaultLevel
    pl = setPosition defaultPlayer (playerSpawn lvl)
    ghosts = [blinky, pinky, inky, clyde]

-- | The specified minimal duration between each game tick
tickDurationIn :: Time
tickDurationIn = 30

-- | Logic for collisions
checkCollisions :: GameState -> GameState
checkCollisions = checkGhostCollisions . checkItemCollisions

checkGhostCollisions :: GameState -> GameState
checkGhostCollisions gs =
  if any (player gs `collides`) $ ghosts gs
    then respawnPlayer . reduceLife $ gs
    else gs
  where
    reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
    respawnPlayer gs
      | isAlive . lives $ player gs = gs {player = (player gs) {position = playerSpawn . level $ gs}}
      | otherwise = gs {status = Lost}

checkItemCollisions :: GameState -> GameState
checkItemCollisions gs = foldr removeItem gs (filter (player gs `collides`) (items . level $ gs))
  where
    removeItem item@(I.Dot pos pts) gs = gs {level = (level gs) {items = filter (/= item) (items . level $ gs)}}
    removeItem item@(I.PowerPellet pos pts) gs = gs {level = (level gs) {items = filter (/= item) (items . level $ gs)}}
    removeItem item@(I.Fruit pos _ pts) gs = gs {level = (level gs) {items = filter (/= item) (items . level $ gs)}}
  -- Add score on remove item
  -- Check if item is power pellet, if so, set ghost state to weak
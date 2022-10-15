module Model.Game
  ( GameState (..),
    defaultGame,
    Status (..),
    Time,
    tickDurationIn,
    checkCollisions,
    checkGameOver,
  )
where

import Model.Ghosts (Ghost (lifeState, mode), blinky, clyde, inky, pinky, GhostState (Frightened), LifeState (Alive, Eaten))
import qualified Model.Items as I
import Model.Level (Level (items, playerSpawn), defaultLevel)
import Model.Movement (Collidable (collides), Direction (..), Positioned (setPosition), Movable (getSpeed))
import Model.Player (Player (lives), defaultPlayer, isAlive, position, rmLife)
import Model.Score (Points)
import Prelude hiding (Left, Right)

-- | Time the game or the tickTimer has been running in milliseconds
type Time = Int

-- | Acitivity status of the game
data Status = Waiting | Active | Paused | GameOver deriving (Eq, Show)

-- | State of the complete game
data GameState = GameState
  { status :: Status,
    player :: Player,
    level :: Level,
    elapsedTime :: Time,
    tickTimer :: Time,
    direction :: Direction,
    bufDirection :: Direction,
    ghosts :: [Ghost],
    points :: Points
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
      ghosts = ghosts,
      points = 0
    }

defaultGame :: GameState
defaultGame = loadGame lvl ghosts pl
  where
    lvl = defaultLevel
    pl = setPosition defaultPlayer (playerSpawn lvl)
    ghosts = [blinky, pinky, inky, clyde]

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
checkCollisions = checkGhostCollisions . checkItemCollisions

checkGhostCollisions :: GameState -> GameState
checkGhostCollisions gs = handleGhostCollisions gs (filter (\g -> player gs `collides` g && lifeState g == Alive) $ ghosts gs)
  -- let collidingGhosts = filter (player gs `collides`) $ ghosts gs
  --  in if not (null collidingGhosts)
  --       then respawnPlayer . reduceLife $ gs
  --       else gs
  -- where
  --   reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
  --   respawnPlayer gs
  --     | isAlive . lives $ player gs = gs {player = (player gs) {position = playerSpawn . level $ gs}}
  --     | otherwise = gs {status = GameOver}

handleGhostCollisions :: GameState -> [Ghost] -> GameState
handleGhostCollisions gs [] = gs
handleGhostCollisions gs (g : _) = if (lifeState g == Alive) && (mode g == Frightened)
  then gs {ghosts = map (\x -> if x == g then x {lifeState = Eaten} else x) (ghosts gs)}
  else respawnPlayer . reduceLife $ gs
  where
    eatGhost g = g {lifeState = Eaten}
    reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
    respawnPlayer gs
      | isAlive . lives $ player gs = gs {player = (player gs) {position = playerSpawn . level $ gs}}
      | otherwise = gs {status = GameOver}

checkItemCollisions :: GameState -> GameState
checkItemCollisions gs = foldr (\item -> removeItem item . addItemScore item . handleItemType item) gs (filter (player gs `collides`) (items . level $ gs))
  where
    removeItem item gs = gs {level = (level gs) {items = filter (/= item) (items . level $ gs)}}
    addItemScore item gs = gs {points = points gs + I.points item}
    handleItemType item gs = case item of
      I.PowerPellet _ _  -> gs {ghosts = map (\x -> x {mode = Frightened}) (ghosts gs)}
      _ -> gs
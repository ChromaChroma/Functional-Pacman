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
    checkFruitSpawning,
  )
where

import Model.Ghosts hiding (position)
import Model.Items (PointItem (Dot, Fruit), fruitOfLevel)
import qualified Model.Items as I
import Model.Level (Level (items, layout, levelNumber, playerSpawn), LevelSize, defaultLevel, layoutSize, tileAtW, Tile (Floor))
import Model.Movement (Collidable (collides), Movable (getSpeed), Positioned (setPosition, getPosition), intPosition)
import Model.Player (Player (lives), defaultPlayer, isAlive, position, rmLife)
import Model.Score (Points)
import Model.Dijkstra
import System.Random (Random (randomR), StdGen, newStdGen)

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
  { ranGen :: StdGen,
    status :: Status,
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

loadGame :: StdGen -> Level -> [Ghost] -> Player -> GameState
loadGame gen lvl ghosts pl =
  GameState
    { ranGen = gen,
      status = Active,
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
      I.PowerPellet _ _ -> gs {ghostMode = Frightened, frightenedTime = 0, ghosts = turnGhostsAround (ghosts gs)}
      _ -> gs

checkGhostCollisions :: GameState -> GameState
checkGhostCollisions gs = handleGhostCollisions gs (filter (`collidesWithMovable` player gs) $ ghosts gs)

handleGhostCollisions :: GameState -> [Ghost] -> GameState
handleGhostCollisions gs [] = gs
handleGhostCollisions gs (g : _) =
  if ghostMode gs == Frightened && isNotEaten g
    then eatGhost
    else respawnPlayer . reduceLife $ gs
  where
    eatGhost = gs {points = points gs + calcGhostPoints updatedGhosts, ghosts = updatedGhosts}
    updatedGhosts = map (\x -> if x == g then x {eatenState = Eaten} else x) (ghosts gs)

    reduceLife gs = gs {player = (player gs) {lives = rmLife . lives . player $ gs}}
    respawnPlayer gs
      | isAlive . lives $ player gs = gs {player = (player gs) {position = playerSpawn . level $ gs}}
      | otherwise = gs {status = GameOver}

calcGhostPoints :: [Ghost] -> Points
calcGhostPoints ghosts
  | eatenGhosts == 0 = 0
  | otherwise = 200 * 2 ^ (eatenGhosts - 1)
  where
    eatenGhosts = length $ filter isEaten ghosts

checkFruitSpawning :: GameState -> GameState
checkFruitSpawning gs
  | noFruitSpawned && shouldSpawnFruit = spawnFruit gs
  | otherwise = gs
  where
    itms = items . level $ gs
    noFruitSpawned = null ([x | x@Fruit {} <- itms])
    shouldSpawnFruit = amountOfDots `mod` 82 == 0 -- Spawn fruit every 82 dots eaten
    amountOfDots = length [x | x@Dot {} <- itms]

spawnFruit :: GameState -> GameState
spawnFruit gs = gs {level = lvl {items = fruit : items lvl}, ranGen = g}
  where
    lvl = level gs
    fruit = setPosition (fruitOfLevel . levelNumber $ lvl) pos
    (pos, g) = randomPos (ranGen gs) gs

    randomPos :: StdGen -> GameState -> ((Float, Float), StdGen)
    randomPos gen gs
      | valid = (rPos, g'')
      | otherwise = randomPos g'' gs
      where
        valid = shortestPath lvl (intPosition rPos) playerPosition /= Infinity
        playerPosition = intPosition (getPosition . player $ gs)

        (x, y) = layoutSize . layout $ lvl
        rPos = (fromIntegral x', fromIntegral y')
        (x', g') = randomR (0, x - 1) gen
        (y', g'') = randomR (0, y - 1) g'

-------------------------------------------------------------------------------
-- Default value functions
-------------------------------------------------------------------------------

frightenedDuration :: Time
frightenedDuration = 5000

defaultGame :: IO GameState
defaultGame = do
  let lvl = defaultLevel
  let ghosts = [blinky, pinky, inky, clyde]
  let pl = setPosition defaultPlayer (playerSpawn lvl)
  generator <- newStdGen
  return (loadGame generator lvl ghosts pl)

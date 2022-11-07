module Model.Game
  ( GameState (..),
    defaultGame,
    reset,
    Status (..),
    GhostMode (..),
    Time,
    tickDurationIn,
    checkCollisions,
    checkGameOver,
    frightenedDuration,
    checkFruitSpawning,
    loadNextLevel,
    addNewScore,
  )
where

import Model.Dijkstra
import Model.Ghosts hiding (direction, position)
import Model.Items (PointItem (Dot, Fruit), fruitOfLevel)
import qualified Model.Items as I
import Model.Level (Level (items, layout, levelNumber, playerSpawn), LevelSize, Tile (Floor), defaultLevel, layoutSize, tileAtW)
import Model.Movement (Collidable (collides), Movable (getSpeed), Positioned (getPosition, setPosition), intPosition, Direction(Stop))
import Model.Player (Player (bufDirection, direction, lives), defaultPlayer, isAlive, position, rmLife)
import Model.Score
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
    ghostMode :: GhostMode,
    highScores :: HighScores
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------
reset :: GameState -> GameState
reset gs = loadGame (ranGen gs) (highScores gs)

loadGame :: StdGen -> HighScores -> GameState
loadGame gen highScores =
  let ghosts = defaultGhosts
      lvl = defaultLevel
      pl = setPosition defaultPlayer (playerSpawn lvl)
   in GameState
        { ranGen = gen,
          status = Waiting,
          elapsedTime = 0,
          tickTimer = 0,
          player = pl,
          level = lvl,
          ghosts = ghosts,
          points = 0,
          frightenedTime = 0,
          ghostMode = Chasing,
          highScores = highScores
        }

loadNextLevel :: GameState -> GameState
loadNextLevel gs =
  let lvl = level gs
      p = player gs
   in gs
        { level = nextLevel lvl,
          player = respawnPlayer p lvl,
          frightenedTime = 0,
          ghostMode = Scatter, --TODO: GHOSTMODE LATEN VARIEREN
          ghosts = defaultGhosts
        }

nextLevel :: Level -> Level
nextLevel lvl = defaultLevel {levelNumber = levelNumber lvl + 1}

addNewScore :: String -> GameState -> GameState
addNewScore name gs = case mkScore name $ points gs of
  Just s -> gs {highScores = addScore s $ highScores gs}
  Nothing -> error "Invalid score"

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
      I.PowerPellet _ _ -> case ghostMode gs of
          Frightened -> gs {frightenedTime = 0, ghosts = turnGhostsAround (ghosts gs)} --ghosts don't need to be slowed down again
          _          -> gs {ghostMode = Frightened, frightenedTime = 0, ghosts = startFrightened (ghosts gs)}
      _ -> gs

checkGhostCollisions :: GameState -> GameState
checkGhostCollisions gs = handleCollidingGhosts gs . filter (`collidesWithMovable` player gs) $ ghosts gs
  where
    handleCollidingGhosts :: GameState -> [Ghost] -> GameState
    handleCollidingGhosts gs ghosts
      | ghostMode gs /= Frightened = foldr (\_ gs' -> killPlayer gs') gs ghosts
      | otherwise = foldr eatGhost gs (filter isNotEaten ghosts)

    killPlayer :: GameState -> GameState
    killPlayer gs =
      let p = player gs
          pLives = lives p
          verifyAlive gs = if isAlive pLives then gs else gs {status = GameOver}
       in verifyAlive $ gs {player = (respawnPlayer p $ level gs) {lives = rmLife pLives}}

    eatGhost :: Ghost -> GameState -> GameState
    eatGhost g gs =
      let updatedGhosts = map (\x -> if x == g then x {eatenState = Eaten} else x) (ghosts gs)
       in gs {points = points gs + calcGhostPoints updatedGhosts, ghosts = updatedGhosts}

respawnPlayer :: Player -> Level -> Player
respawnPlayer p lvl = p {position = playerSpawn lvl, direction = Stop, bufDirection = Stop}

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
    shouldSpawnFruit = amountOfDots /= 0 && amountOfDots `mod` 82 == 0 -- Spawn fruit every 82 dots eaten
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
  let highScores = mkHighScores []
  generator <- newStdGen
  return (loadGame generator highScores)

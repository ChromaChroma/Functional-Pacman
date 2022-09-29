module Model.Characters where 

-- | Directions a movement can be in, including Stop for an idle in movement
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)

-- | Name of a Player or Ghost
type Name = String

-- |A movable's Speed in Ints unit
type Speed = Int

-- | A movable's position in Floats
type Position = Float Float -- Floats might be prefered

class Movable a where
    getSpeed    :: Speed
    getPosition :: Position
    move        :: a -> Direction -> a

-- | Default movement handler for a player and ghost
standardMove :: Movable a => a -> Direction -> a    
standardMove player direction = player { position = newPosition }
    where newPosition = case direction of
            Up    -> (fst position,             snd position + getSpeed )
            Down  -> (fst position,             snd position - getSpeed )
            Left  -> (fst position - getSpeed,  snd position            )
            Right -> (fst position + getSpeed,  snd position            )
            Stop  -> position

-- | State of living of a Player or Ghost
data LifeState = Alive | Dead deriving (Eq, Show)

-- | Number of lives the player has left
data Lives = Lives {
    lives :: Int
    alive :: LifeState
} deriving (Eq, Show)

-- | Safe Lives constructor
mkLives :: Int -> Maybe Lives
mkLives | < 0 = Nothing
        | otherwise = Just Lives

-- | Remove a life from the Player's lives
rmLife :: Lives -> Lives
rmLife lives {lives = life} | life > 0 = lives {lives = life - 1}
                            | otherwise = lives { lives = life - 1, alive = Dead}

-- | A Player's state
-- | Normal is the players default state
-- | Strong is the state the player is in when he eats a power pellet and when he can attakc the ghosts
data PlayerState = Normal | Strong deriving (Eq, Show)

data Ghost = Player {
    name        :: PlayerName
    playerState :: PlayerState
    speed       :: Speed
    position    :: windowOffsetPosition
    lives       :: Lives
} 

-- | The player's Movable implementation
instance Movable Player where
    getSpeed = speed
    getPosition = position
    move player direction = defaultMovementHandler player direction

-- | States a ghost can be in
-- | Chasing is the state in which ghosts chase the player
-- | Frightend is the state in which ghosts run away from the player
-- | Scatter is the state in which ghosts move to a specific location
data EnemyState = Chasing | Frightend | Scatter deriving (Eq, Show) -- of Vulnerable

-- | The ghost's current state
data Ghost = Ghost {
    name      :: Name
    mode      :: EnemyState
    speed     :: Speed
    position  :: Position
    alive     :: LifeState
} 

-- | The ghost's Movable implementation
instance Movable Ghost where
    getSpeed = speed
    getPosition = position
    move ghost direction = defaultMovementHandler ghost direction 
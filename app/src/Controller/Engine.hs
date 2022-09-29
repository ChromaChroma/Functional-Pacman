module Engine(Engine) where

-- | The Engine that is the interface for the internal game state, logic etc.
-- | The engine is responsible for the game loop, and the game state.
-- | Class interface of a game engine to be implemented
class Engine {
    -- type State
    -- type Action
    -- type Result
    -- type Error
    
} where
    -- | The initial state of the game
    init :: State
    -- | Run provided action on the game state
    step :: State -> Action -> Result
    -- | Update game state using provided result
    update :: State -> Result -> State
    -- | Check if the game is over
    error :: State -> Error -> State

-- | Example Actions
data Action = Move | Attack | Defend | Cast | Use | Talk | PickUp | Drop | Inventory | Equip | Unequip | Rest | Save | Load | Quit deriving (Eq, Show)

-- | Example Results
data Result = MoveResult | AttackResult | DefendResult | CastResult | UseResult | TalkResult | PickUpResult | DropResult | InventoryResult | EquipResult | UnequipResult | RestResult | SaveResult | LoadResult | QuitResult deriving (Eq, Show)

-- | Example Errors
data Error = InvalidAction | InvalidResult | InvalidState | InvalidError  deriving (Eq, Show)


-- -- Constant timer used by the game to activate next game tick
-- constantTimer :: Int
-- constantTimer = undefined

-- | Instance implementation of the engine. 
-- instance Engine GameEngine = {
instance Engine GameState {
    -- ... The game state fields
    -- constantTimer = constantTimer
} where
    init = GameState { undefined }

    step state action = undefined 

    update state result  = undefined 

    error state error = undefined




-- Some semiconstant timer? that updates game state x times per second (checks movement of player/ enemies and updates them / runs their AI)

-- | Global Game State
data ActivityState = Waiting | Active | Paused | Won | Lost deriving (Eq, Show)

data GameState = GameState {
    state :: ActivityState
    player :: Player
    enemies :: [Enemy]
    level :: Level
    -- etc
}

-- | Level 
type LevelNumber = Int
-- | Matrix like level layout. Misschien dit, maar misschien (eerst) 
-- | locaties van view en de logica ervan als een dinge houden en dan opslitsen naar de engine?
type LevelLayout = [[Tile]]
data Level = Level {
    id :: LevelNumber
    layout :: LevelLayout
    -- etc
}

data Tile = Wall | Floor | Pellet | PowerPellet deriving (Eq, Show)
-- Maybe?? : Enemy | Player


-- | Todo: Wellicht overzetten naar data ipv classes. Is meer de functionele stijl dan ipv een imperative coding style
-- | (zie: https://wiki.haskell.org/Typeclassopedia#Data_types_instead_of_type_classes)
-- | Movement and Entities
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)
type Speed = Int
type Position = Int Int -- Floats might be prefered
class Movable a where
    getSpeed :: Speed
    getPosition :: Position
    move :: a -> Direction -> a

data PlayerState = Normal | Strong deriving (Eq, Show)
instance Movable Player = {
    name :: String
    playerMode :: PlayerState
    speed :: Speed
    position :: Position
    -- etc
} where
    getSpeed = speed

    getPosition = position
    
    move player direction = player { position = newPosition }
        where newPosition = case direction of
                Up -> (fst position, snd position + getSpeed)
                Down -> (fst position, snd position - getSpeed)
                Left -> (fst position - getSpeed, snd position)
                Right -> (fst position + getSpeed, snd position)
                Stop -> position

data EnemyState = Aggressive | Passive | Frightend deriving (Eq, Show)
instance Movable Enemy = {
    name :: String
    mode :: EnemyState
    -- etc
} where
    getSpeed = speed

    getPosition = position
    
    move enemy direction = enemy { position = newPosition }
        where newPosition = case direction of
                Up -> (fst position, snd position + getSpeed)
                Down -> (fst position, snd position - getSpeed)
                Left -> (fst position - getSpeed, snd position)
                Right -> (fst position + getSpeed, snd position)
                Stop -> position



-- | Scores
data HighScores = HighScores [Score]

getFirstPlace :: HighScores -> Maybe Score
getFirstPlace [] = Nothing
getFirstPlace = Just . head . sort

data Score = Score{
    name :: String
    score :: Int
    -- etc
}
instance Ord Score where
    compare = compare `on` score

instance Eq Score where
    (==) = (==) `on` score

mkScore :: String -> Int -> Maybe Score
mkScore name score 
    | score < 0 = Nothing
    | otherwise = Just Score { name = name, score = score }
-- mkScore name score = case score of 
--     score | score < 0 -> Nothing
--     _ -> Just Score { name = name, score = score }


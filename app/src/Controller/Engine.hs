module Engine(Engine) where

-- | The Engine that is the interface for the internal game state, logic etc.
-- | The engine is responsible for the game loop, and the game state.
-- | Class interface of a game engine to be implemented
class Engine {
    -- type State
    -- type Action
    -- type Result
    -- type Error

    -- | The initial state of the game
    init :: State
    
    -- | Run provided action on the game state
    step :: State -> Action -> Result

    -- | Update game state using provided result
    update :: State -> Result -> State

    -- | Check if the game is over
    error :: State -> Error -> State
    
} where

-- | Example Actions
data Action = Move | Attack | Defend | Cast | Use | Talk | PickUp | Drop | Inventory | Equip | Unequip | Rest | Save | Load | Quit deriving (Eq, Show)

-- | Example Results
data Result = MoveResult | AttackResult | DefendResult | CastResult | UseResult | TalkResult | PickUpResult | DropResult | InventoryResult | EquipResult | UnequipResult | RestResult | SaveResult | LoadResult | QuitResult deriving (Eq, Show)

-- | Example Errors
data Error = InvalidAction | InvalidResult | InvalidState | InvalidError  deriving (Eq, Show)

-- | Instance implementation of the engine. 
instance Engine GameEngine = {
    -- constantTimer = constantTimer

    init = GameState { undefined }

    step state action = undefined 

    update state result  = undefined 

    error state error = undefined
}
-- -- Constant timer used by the game to activate next game tick
-- constantTimer :: Int
-- constantTimer = undefined



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

-- | Matrix like level layout. Misschien dit, maar misschien (eerst) 
-- | locaties van view en de logica ervan als een dinge houden en dan opslitsen naar de engine?
type LevelLayout = [[Tile]]
data Level = Level {
    id :: Int
    layout :: LevelLayout
    -- etc
}

data Tile = Wall | Floor | Pellet | PowerPellet deriving (Eq, Show)
-- Maybe?? : Enemy | Player


-- | Todo: Wellicht overzetten naar data ipv classes. Is meer de functionele stijl dan ipv een imperative coding style
-- | (zie: https://wiki.haskell.org/Typeclassopedia#Data_types_instead_of_type_classes)
-- | Movement and Entities
data Direction = Up | Down | Left | Right | Stop deriving (Eq, Show)

class Movable a {
    speed :: Int
    position :: (Int, Int) -- Floats might be prefered
    move :: a -> Direction -> a
} where


instance Movable Player = {
    name :: String
    playerMode :: PlayerMode
    -- etc
}
data PlayerMode = Normal | Strong deriving (Eq, Show)

instance Movable Enemy = {
    name :: String
    mode :: EnemyMode
    -- etc
}
data EnemyMode = Aggressive | Passive | Frightend deriving (Eq, Show)





-- | Scores
data HighScores = HighScores [Score]

data Score = Score{
    name :: String
    score :: Int
    -- etc
}


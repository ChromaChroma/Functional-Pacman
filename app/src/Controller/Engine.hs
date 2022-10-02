module Controller.Engine where

import Model.Characters
import Model.Game hiding (player)
import Model.Level


startNewGame :: IO GameState
startNewGame = undefined

step :: Int -> GameState -> IO GameState
step ms gs  | status gs == Active && elapsedTime gs + ms > tickDurationInMs = do
                -- Game flow ran each tick
                -- Check/Update Player state
                -- Move player
                -- Check/Move Ghosts (AI)
                -- Check game over
                return $ resetElapsedTime
                  . checkGameOver
                  . updateGhosts
                  . updateGhosts
                  . updatePlayer $ gs
            | otherwise = return gs { elapsedTime = elapsedTime gs + ms }


-- | Update player position and state (Normal/Strong)
updatePlayer :: GameState -> GameState
updatePlayer gs = undefined

-- | Update ghosts position and state (Chase / Scatter / Frightened)
updateGhosts :: GameState -> GameState
updateGhosts gs = undefined

-- | Check if game is over and update it if necessary
checkGameOver :: GameState -> GameState
checkGameOver gs = undefined

-- | Reset elapsed time to 0 for next tick cycle
resetElapsedTime :: GameState -> GameState
resetElapsedTime gs = gs { elapsedTime = 0 }

-- |
-- | Game Input functions
-- |
-- | Change player's direction / stop
movePlayer :: Direction -> GameState -> GameState
movePlayer dir gs = gs {level = makeMove (level gs) dir}
  where 
    makeMove :: Level -> Direction -> Level
    makeMove lvl dir = lvl { player = move (player lvl) dir}

    -- level = level gs
    -- pl = player level
    -- newLevel lvl = lvl {player = }

--  Pause the game
pause :: GameState -> GameState
pause gs  | status gs == Active = gs { status = Paused }
          | otherwise = gs

-- | Resume the game
resume :: GameState -> GameState
resume gs | status gs == Paused = gs { status = Active }
          | otherwise = gs

-- | End the game (forfeiting the current game)
quit :: GameState -> GameState
quit gs = gs { status = Lost }


-- Not actions fot the game itself
-- save :: a -> a
-- load :: a -> a

-- -- Class with all Actions that can be taken onto the game state
-- class GameState a => GameEngine a where
--     -- | Move the player in the given direction
--     movePlayer  :: Direction -> a -> a
--     -- | Pause the game
--     pause       :: a -> a
--     -- | Resume the game
--     resume      :: a -> a

-- instance GameEngine GameState where
--     movePlayer direction gs = gs { player = move direction (player gs) }

--     pause gs | status gs == Active = gs { status = Paused }
--              | otherwise = gs

--     resume gs | status gs == Paused = gs { status = Active }
--               | otherwise = gs

-- | The Engine that is the interface for the internal game state, logic etc.
-- | The engine is responsible for the game loop, and the game state.
-- | Class interface of a game engine to be implemented
-- class Engine a where
--     -- | The initial state of the game
--     init :: a
--     -- | Run provided action on the game state
--     step :: a -> Action -> Result
--     -- | Update game state using provided result
--     update :: a -> Result -> State
--     -- | Check if the game is over
--     error :: a -> Error -> a

-- | The action that is provided to the engine
-- data Action = 
--     Move Direction 
--   | Pause 
--   | Continue 
--   | Quit 
--   | Save 
--   | Load  
--   deriving (Show, Eq) 
-- !! Quit save and load might not be actions of Engine but of view / other controller
-- 
-- -- | Example Results
-- data Result = MoveResult | AttackResult | DefendResult | CastResult | UseResult | TalkResult | PickUpResult | DropResult | InventoryResult | EquipResult | UnequipResult | RestResult | SaveResult | LoadResult | QuitResult deriving (Eq, Show)
-- 
-- -- | Example Errors
-- data Error = InvalidAction | InvalidResult | InvalidState | InvalidError  deriving (Eq, Show)
-- 
-- -- -- Constant timer used by the game to activate next game tick
-- -- constantTimer :: Int
-- constantTimer = undefined

-- -- | Instance implementation of the engine. 
-- -- instance Engine GameEngine = {
-- instance Engine GameState {
--     -- ... The game state fields
--     -- constantTimer = constantTimer
-- } where
--     init = GameState { undefined }
--     step state action = undefined 
--     update state result  = undefined 
--     error state error = undefined



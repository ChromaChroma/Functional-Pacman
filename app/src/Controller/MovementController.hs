module Controller.MovementController where
import Model.Game
import Model.Movement
import Model.Level
import Model.Characters
import Prelude hiding (Left, Right, Up, Down)

import Data.Fixed (mod')
import Numeric 
import Data.Maybe

makePlayerMove :: GameState -> GameState
makePlayerMove gs 
  | isJust bufMovedPlayer = gs {player = fromJust bufMovedPlayer, direction = bufDirection gs, bufDirection = Stop}
  | isJust movedPlayer = gs {player = fromJust movedPlayer}
  | otherwise = gs
  where
    bufMovedPlayer = makeDirectionMove gs (bufDirection gs)
    movedPlayer =  makeDirectionMove gs (direction gs)
    
makeDirectionMove :: GameState -> Direction -> Maybe Player
makeDirectionMove gs dir  
  | dir /= Stop && canMakeMoveToDir plPos dir && isValidPlayerPosition lvl movedPlayer 
    = Just movedPlayer
  | otherwise = Nothing
  where
    movedPlayer  = move pl dir

    pl = player gs
    plPos = pPosition pl
    lvl = level gs

-- Check for tile in next direction (x+1,y)
-- Stop movement on crossroads (0.1 diff)
canMakeMoveToDir :: Position -> Direction -> Bool
canMakeMoveToDir (x, y) dir = case dir of
  Up    -> isWholeFixedTwo x  --Is player on whole number x position
  Down  -> isWholeFixedTwo x  --Is player on whole number x position
  Left  -> isWholeFixedTwo y  --Is player on whole number y position
  Right -> isWholeFixedTwo y  --Is player on whole number y position
  Stop  -> True
  where
    isWholeFixedTwo x = 0.0 == formatFloat x || formatFloat x == 0.9
    formatFloat x = read (showFFloat (Just 2) (x `mod'` 1) "" ) :: Float
    
    -- isWholeFixedTwo x = "0.00" == (formatFloat x)
    -- formatFloat x = showFFloat (Just 2) (2.005 `mod'` 1) ""

    -- isWholeFixedTwo x = "0.0" == (formatFloat x) || (formatFloat x) == "0.9"
    -- formatFloat x = showFFloat (Just 2) (2.005 `mod'` 1) ""

isValidPlayerPosition :: Level -> Player -> Bool
isValidPlayerPosition = isValidMovablePosition isValid
  where
    isValid Floor = True 
    isValid _ = False

-- Higher order function to check a movables position with a provided Tile predicate
isValidMovablePosition :: Movable a => (Tile -> Bool) -> Level -> a -> Bool
isValidMovablePosition p level m = case tileAt level intPPos of
      Just t -> p t
      _ -> False  --Out of bounds is invalid move when not wrapping movement
  where
    intPPos = intPosition (getPosition m)


-- isValidGhostPosition :: Level -> Ghost -> Bool
-- isValidGhostPosition = isValidPosition isValid
--   where
--     isValid Wall = False
--     isValid (GhostDoor Open) = False
--     isValid (GhostDoor Closed) = True
--     isValid _ = True

-- isValidPosition when using tileAtW
-- isValidPosition :: Movable a => (Tile -> Bool) -> Level -> a -> Bool
-- isValidPosition p level m = p . tileAtW level . intPosition $ getPosition m

-- -- check bufdirection mvoe
-- -- check direction move
-- -- stay sameplace, update dir to stop

-- makeMove :: Movable a => GameState -> a -> a
-- makeMove gs m
--   | isValidPlayerPosition (level gs) normalMove = normalMove
--   | isValidPlayerPosition (level gs) bufMove = normalMove
--   | otherwise = a
--   where
--     (mx, my) = getPosition a -- get current x,y
--     -- check if player is on axis of direction (Up/down = x is whole number, Left/right = y is wholenumber) before checking if can move direction
--     -- This kind of checks if player position is on crossroads
    
--     -- if so (guard cases) check if move in direction is valid like in engine
--     -- if so make move (return new player) else return original player (movable)
--     -- normalMove is 
--     normalMove  = moveFull m $ direction gs
--     bufMove     = moveFull m $ bufDirection gs
    
  
-- moveFull :: Movable a => a -> Direction -> a
-- moveFull m dir = setPosition m (moveFullUnit m dir)
--   where
--     moveFullUnit m dir = case dir of
--       Up -> (x, y -1)
--       Down -> (x, y + 1)
--       Left -> (x -1, y)
--       Right -> (x + 1, y)
--       _ -> (x, y)
--       where
--         (x, y) = getPosition m
--    -- Player with new (x.y) position
-- validateMove = undefined


-- -- moveToPosition :: Movable a => Level -> a -> Maybe a
-- -- moveToPosition level movable =
-- --   where
-- --     (x, y) = getPosition movable
-- --     tile = case tileAt level (x, y) of
-- --       Just t -> t
-- --       Nothing -> -- do wrap check




-- | Try player buf direction and normal direction for a player
-- makePlayerMove :: GameState -> GameState
-- makePlayerMove gs  
--   -- | isValidPlayerPosition lvl bufMovedPlayer  = gs {player = bufMovedPlayer}
--   | bufDir /= Stop  && canMakeMoveToDir plPos bufDir  && isValidPlayerPosition lvl bufMovedPlayer = gs {player = bufMovedPlayer, direction = bufDir, bufDirection = Stop}
--   | dir /= Stop     && canMakeMoveToDir plPos dir     && isValidPlayerPosition lvl movedPlayer = gs {player = movedPlayer}
--   | otherwise = gs
--   where
--     pl = player gs
--     plPos = pPosition pl
--     lvl = level gs

--     bufDir = bufDirection gs
--     bufMovedPlayer  = move pl bufDir
    
--     dir = direction gs
--     movedPlayer = move pl dir
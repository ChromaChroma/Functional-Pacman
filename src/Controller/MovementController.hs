module Controller.MovementController where

import Data.Maybe (Maybe (..), fromJust, isJust)
import Model.Game (GameState (level, player), GhostMode (Frightened))
import Model.Ghosts (Ghost, isEaten)
import Model.Level
  ( Level (layout),
    Tile (Floor, GhostDoor),
    layoutSize,
    tileAtW,
  )
import Model.Movement
  ( Direction (..),
    Movable (move),
    Positioned (getPosition, setPosition),
    intPosition,
  )
import Model.Player (Player (bufDirection, direction))
import Model.Utils
import Prelude hiding (Down, Left, Right, Up)

makePlayerMove :: GameState -> GameState
makePlayerMove gs
  | isJust bufMovedPlayer = gs {player = (fromJust bufMovedPlayer) {direction = bufDir, bufDirection = Stop}}
  | isJust movedPlayer = gs {player = fromJust movedPlayer}
  | otherwise = gs
  where
    bufMovedPlayer = makeDirectionMove gs bufDir
    bufDir = bufDirection . player $ gs
    movedPlayer = makeDirectionMove gs (direction . player $ gs)

makeDirectionMove :: GameState -> Direction -> Maybe Player
makeDirectionMove gs dir
  | dir /= Stop && canMoveInDir && isValidMovePosition =
    Just movedPlayer
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDir pl dir lvl
    isValidMovePosition = isValidPlayerPosition lvl movedPlayer
    movedPlayer = move pl dir (layoutSize . layout $ lvl)
    pl = player gs
    lvl = level gs

canMakeMoveToDir :: Movable a => a -> Direction -> Level -> Bool
canMakeMoveToDir player dir lvl
  | isValid = case dir of
    Up -> canMovePerpendicular x
    Down -> canMovePerpendicular x
    Left -> canMovePerpendicular y
    Right -> canMovePerpendicular y
    Stop -> True
  | otherwise = False
  where
    isValid = isValidPlayerPosition lvl . moveFull player $ dir
    (x, y) = getPosition player

-- | Takes coordinate of axis perpendicular to direction you want to move on
-- | i.e. If you want move vertically, you pass the current x axis coordinate
canMovePerpendicular :: RealFloat a => a -> Bool
canMovePerpendicular n = let nFormat = formatDecimals n 1 in nFormat == 0.0 || nFormat == 1.0

-- | Moves player in direction by the valiadtionOffset to check if Movable can 'stand on' the tile at coordinate
moveFull :: Movable a => a -> Direction -> a
moveFull m dir = setPosition m (moveFullUnit m dir)
  where
    moveFullUnit m dir = case dir of
      Up -> (x, y + validationOffset)
      Down -> (x, y - validationOffset)
      Left -> (x - validationOffset, y)
      Right -> (x + validationOffset, y)
      _ -> (x, y)
      where
        validationOffset = 0.55
        (x, y) = getPosition m

-- | Checks if the player is in a valid position on the level
isValidPlayerPosition :: Movable a => Level -> a -> Bool
isValidPlayerPosition = isValidMovablePosition (== Floor)

-- | Higher order function that checks if a movable is in a valid position on the level based on a provided Tile predicate
isValidMovablePosition :: Movable a => (Tile -> Bool) -> Level -> a -> Bool
isValidMovablePosition p level m = p . tileAtW level . intPosition $ getPosition m

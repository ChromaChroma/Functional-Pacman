module Controller.MovementController where

import Data.Fixed (mod')
import Data.Maybe (Maybe (..), fromJust, isJust)
import Model.Game (GameState (bufDirection, direction, level, player))
import Model.Ghosts (Ghost)
import Model.Level
  ( DoorState (Open),
    Level (layout),
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
import Model.Player (Player)
import Numeric (showFFloat)
import Prelude hiding (Down, Left, Right, Up)

makePlayerMove :: GameState -> GameState
makePlayerMove gs
  | isJust bufMovedPlayer = gs {player = fromJust bufMovedPlayer, direction = bufDirection gs, bufDirection = Stop}
  | isJust movedPlayer = gs {player = fromJust movedPlayer}
  | otherwise = gs
  where
    bufMovedPlayer = makeDirectionMove gs (bufDirection gs)
    movedPlayer = makeDirectionMove gs (direction gs)

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

canMakeMoveToDir :: Player -> Direction -> Level -> Bool
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

-- | Takes float `n` and returns a float of the decimal places rounded to `j` decimals
-- | i.e formatDecimals 9.005 1 = 0.0, formatDecimals 9.05 1 = 0.5
formatDecimals :: RealFloat a => a -> Int -> Float
formatDecimals n j = read (showFFloat (Just j) (n `mod'` 1) "") :: Float

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
isValidPlayerPosition :: Level -> Player -> Bool
isValidPlayerPosition = isValidMovablePosition (== Floor)

-- | Higher order function that hecks if a movable is in a valid position on the level based on a provided Tile predicate
isValidMovablePosition :: Movable a => (Tile -> Bool) -> Level -> a -> Bool
isValidMovablePosition p level m = p . tileAtW level . intPosition $ getPosition m

-- | Checks if the ghost is in a valid position on the level
isValidGhostPosition :: Level -> Ghost -> Bool
isValidGhostPosition = isValidMovablePosition (`elem` [Floor, GhostDoor Open])


module Model.GhostAI where



import Data.Fixed (mod')
import Data.Maybe (Maybe (..), fromJust, isJust)
import Model.Game (GameState (level, ghosts, ghostMode))
import Model.Ghosts (Ghost)
import Model.Level
  ( DoorState (Open, Closed),
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
import Numeric (showFFloat)
import Prelude hiding (Down, Left, Right, Up)



import Model.Ghosts (Ghost (..), Name (..), EatenState (..))
import Controller.MovementController



makeGhostsMove :: GameState -> GameState
makeGhostsMove gs = gs {ghosts = map (make1GhostMove gs) (ghosts gs)}


make1GhostMove :: GameState -> Ghost -> Ghost
make1GhostMove gs ghst
  | isJust movedGhost = fromJust movedGhost
  | otherwise = ghst
  where
    movedGhost = makeDirectionMoveGhost gs ghst (direction ghst)


makeDirectionMoveGhost :: GameState -> Ghost -> Direction -> Maybe Ghost
makeDirectionMoveGhost gs ghst dir
  | canMoveInDir && isValidMovePosition = Just movedGhost
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDir ghst dir lvl
    isValidMovePosition = isValidGhostPosition (ghostMode gs) lvl movedGhost
    movedGhost = move ghst dir (layoutSize . layout $ lvl)
    lvl = level gs





--END


import Model.Ghosts (Ghost (..), Name (..), EatenState (..))
import Controller.MovementController



-- ghostMoveStraight :: Ghost -> Ghost
-- ghostMoveStraight g = move g $ direction g

makeDirectionMoveGhost :: GameState -> Ghost -> Direction -> Maybe Ghost
makeDirectionMove gs ghst dir
  | dir /= Stop && canMoveInDir && isValidMovePosition =
    Just movedGhost
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDir ghst dir lvl
    isValidMovePosition = isValidPlayerPosition lvl movedGhost
    movedPlayer = move ghst dir (layoutSize . layout $ lvl)
    lvl = level gs





--END

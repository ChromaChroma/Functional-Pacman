
module Model.GhostAI where



import Data.Fixed (mod')
import Data.Maybe (Maybe (..), fromJust, isJust)
import Model.Game
import Model.Ghosts
import Model.Level
import Model.Movement
import Numeric (showFFloat)
import Prelude hiding (Down, Left, Right, Up)



import Controller.MovementController






-----------------------------------------------------------------------------

makeGhostsMove :: GameState -> GameState
makeGhostsMove gs = gs {ghosts = map (make1GhostMove gs) (ghosts gs)}


make1GhostMove :: GameState -> Ghost -> Ghost
make1GhostMove gs ghst  -- hierzo een case voor: is de ghost de volgende tile op een intersection?
  = case isJust movedGhost of
      True  -> fromJust movedGhost
      False -> next --KIEST NU DE EERSTE OPTIE -- case length possibledirections == 1: die kant. anders: ghost AI
  where
    movedGhost = makeDirectionMoveGhost gs ghst (direction ghst)
    (next:_) = checkMoveDirs gs ghst

--TODO: ALS CURRENT TILE HUN SPAWNPOINT (OF DE TILE BOVEN/ONDER) IS:
--  OP/NEER BEWEGEN (TERUGKAATSEN), NIET NAAR LINKS/RECHTS


canMakeMoveToDirGhost :: GameState -> Ghost -> Direction -> Level -> Bool
canMakeMoveToDirGhost gs gh dir lvl
  | isValid = case dir of
    Up -> canMovePerpendicular x
    Down -> canMovePerpendicular x
    Left -> canMovePerpendicular y
    Right -> canMovePerpendicular y
    Stop -> True
  | otherwise = False
  where
    isValid = isValidGhostPosition (ghostMode gs) lvl (moveFull gh dir)
    (x, y) = getPosition gh

makeDirectionMoveGhost :: GameState -> Ghost -> Direction -> Maybe Ghost
makeDirectionMoveGhost gs ghst dir
  | canMoveInDir && isValidMovePosition = Just updatedGhost
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDirGhost gs ghst dir lvl
    isValidMovePosition = isValidGhostPosition (ghostMode gs) lvl movedGhost

    updatedGhost = movedGhost {direction = dir, opDirection = opp dir}

    movedGhost = move ghst dir (layoutSize . layout $ lvl)
    lvl = level gs


checkMoveDirs :: GameState -> Ghost -> [Ghost]
checkMoveDirs gs gh
  = case possiblemoves of
        [] -> [fromJust (makeDirectionMoveGhost gs gh (opDirection gh))] --terug als er niks anders is (deadend)
        _  -> possiblemoves
  where
    possiblemoves = map fromJust $ filter (isJust) [makeDirectionMoveGhost gs gh x | x <- u]
    u = filter (/= opDirection gh) [Up,Left,Down,Right]

-- | Checks if the ghost is in a valid position on the level
isValidGhostPosition :: GhostMode -> Level -> Ghost -> Bool
isValidGhostPosition gm lvl gh = isValidMovablePosition (`elem` validTiles) lvl gh
  where
    validTiles = case gm of
      Frightened -> [Floor]
      _ -> case isEaten gh of
            True  -> [Floor, GhostDoor Open, GhostDoor Closed]
            False -> case direction gh of
                      Up    -> [Floor, GhostDoor Open, GhostDoor Closed]
                      _     -> [Floor]


--TODO: ESCAPE-FUNCTIE VOOR EEN GHOST: BEGIN MET NAAR BOVEN BEWEGEN ()
-- VERDER: ALS GHOST GEGETEN WORDT WEER LATEN TERUGKEREN NAAR SPAWN MET DIRECTION = STOP
--  EN NA KORTE TIJD WEER DE ESCAPE-FUNCTIE AANROEPEN














--END

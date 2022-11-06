
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
  | isJust nextintersect == True = fromJust movedGhostOpp
  | otherwise = case isJust movedGhost of
                  True  ->  fromJust movedGhost
                  False ->  case length movedirs of
                              1 -> head movedirs --KIEST NU DE EERSTE OPTIE -- case length possibledirections == 1: die kant. anders: ghost AI
                              _ -> head movedirs --als de ghost 2 kanten op kan: GHOST AI
    where
      movedGhost = makeDirectionMoveGhost gs ghst (direction ghst)
      movedGhostOpp = makeDirectionMoveGhost gs ghst (opDirection ghst)
      movedirs = checkMoveDirs gs ghst

      nextintersect = checkMoveToIntersection gs ghst

--TODO: ALS CURRENT TILE HUN SPAWNPOINT (OF DE TILE BOVEN/ONDER) IS:
--  OP/NEER BEWEGEN (TERUGKAATSEN), NIET NAAR LINKS/RECHTS
-- (11,15)   - (16 17)
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


checkMoveDirs :: GameState -> Ghost -> [Ghost]
checkMoveDirs gs gh
  = case elem (tileX, tileY) [(u,15) | u <- [13..16]] of
      True -> [fromJust (makeDirectionMoveGhost gs gh (opDirection gh))]
      False -> case possiblemoves of
                [] -> [fromJust (makeDirectionMoveGhost gs gh (opDirection gh))] --terug als er niks anders is (deadend)
                _  -> possiblemoves
  where
    (x, y) = getPosition gh
    (tileX, tileY) = (round x, round y)

    possiblemoves = map fromJust $ filter (isJust) [makeDirectionMoveGhost gs gh x | x <- u]
    u = filter (/= opDirection gh) [Up,Left,Down,Right]

checkMoveToIntersection :: GameState -> Ghost -> Maybe (Int, Int)
checkMoveToIntersection gs gh
  | elem nextpoint intersections = Just nextpoint
  | otherwise                    = Nothing
  where
    (x,y)     = getPosition gh
    (x',y')   = (round x, round y) --de tile waar de ghost op staat
    nextpoint = case direction gh of
                  Up    -> (x', y' + 1)
                  Down  -> (x', y' - 1)
                  Left  -> (x' - 1, y')
                  Right -> (x' + 1, y')
                  Stop -> (x', y')
    intersections = [ (a,b) | (a,b) <- (levelIntersections . level $ gs),
                              not (elem (a,b) [(c,d) | c <- [11..16], d <- [15..17]] ),
                              not (elem (a,b) [(12,7), (15,7), (12,19), (15,19)] && direction gh /= Down ) ]
                              -- above: if the ghost happens to be going to one of the non-intersection tiles (4 of them)
                              -- while moving up left or right, it will not see it as an intersection (moving down, it will)


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

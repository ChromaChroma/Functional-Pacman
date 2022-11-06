
module Model.GhostAI where



import Data.Fixed (mod')
import Data.Maybe (Maybe (..), fromJust, isJust)
import Model.Game
import Model.Player as P
import Model.Ghosts as G
import Model.Level
import Model.Movement
import Numeric (showFFloat)
import Prelude hiding (Down, Left, Right, Up)



import Controller.MovementController






-----------------------------------------------------------------------------

makeGhostsMove :: GameState -> GameState
makeGhostsMove gs = gs {ghosts = map (make1GhostMove gs) (ghosts gs)}


make1GhostMove :: GameState -> Ghost -> Ghost
make1GhostMove gs ghst  -- hierzo een case voor: is de ghost de HUIDIGE tile op een intersection? zo ja, die kant op gaan.
  | isJust nextintersect == True = fromJust movedGhostOpp -- hier de nextDirection van de ghost instellen
  | otherwise = case isJust movedGhost of
                  True  ->  fromJust movedGhost
                  False ->  case length movedirs of
                              1 -> head movedirs --KIEST NU DE EERSTE OPTIE -- case length possibledirections == 1: die kant. anders: ghost AI
                              _ -> head movedirs --als de ghost 2 kanten op kan: GHOST AI
    where
      movedGhost = makeDirectionMoveGhost gs ghst (G.direction ghst)
      movedGhostOpp = makeDirectionMoveGhost gs ghst (opDirection ghst)
      movedirs = checkMoveDirs gs ghst

      nextintersect = checkMoveToIntersection gs ghst

--TODO: ALS CURRENT TILE HUN SPAWNPOINT (OF DE TILE BOVEN/ONDER) IS:
--  OP/NEER BEWEGEN (TERUGKAATSEN), NIET NAAR LINKS/RECHTS
-- (11,15)   - (16 17)

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

makeDirectionMoveGhost :: GameState -> Ghost -> Direction -> Maybe Ghost
makeDirectionMoveGhost gs ghst dir
  | canMoveInDir && isValidMovePosition = Just updatedGhost
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDirGhost gs ghst dir lvl
    isValidMovePosition = isValidGhostPosition (ghostMode gs) lvl movedGhost

    updatedGhost = movedGhost {G.direction = dir, opDirection = opp dir}

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

-- | Checks if the ghost is in a valid position on the level
isValidGhostPosition :: GhostMode -> Level -> Ghost -> Bool
isValidGhostPosition gm lvl gh = isValidMovablePosition (`elem` validTiles) lvl gh
  where
    validTiles = case gm of
      Frightened -> [Floor]
      _ -> case isEaten gh of
            True  -> [Floor, GhostDoor Open, GhostDoor Closed]
            False -> case G.direction gh of
                      Up    -> [Floor, GhostDoor Open, GhostDoor Closed]
                      _     -> [Floor]

checkMoveToIntersection :: GameState -> Ghost -> Maybe (Int, Int)
checkMoveToIntersection gs gh
  | elem nextpoint intersections = Just nextpoint
  | otherwise                    = Nothing
  where
    (x,y)     = getPosition gh
    (x',y')   = (round x, round y) --de tile waar de ghost op staat
    nextpoint = case G.direction gh of
                  Up    -> (x', y' + 1)
                  Down  -> (x', y' - 1)
                  Left  -> (x' - 1, y')
                  Right -> (x' + 1, y')
                  Stop -> (x', y')
    intersections = [ (a,b) | (a,b) <- (levelIntersections . level $ gs),
                              not (elem (a,b) [(c,d) | c <- [11..16], d <- [15..17]] ),
                              not (elem (a,b) [(12,7), (15,7), (12,19), (15,19)] && G.direction gh /= Down ) ]
                              -- above: if the ghost happens to be going to one of the non-intersection tiles (4 of them)
                              -- while moving up left or right, it will not see it as an intersection (moving down, it will)


targetTileGhost :: GameState -> Ghost -> (Int, Int)
targetTileGhost gs gh = case ghostMode gs of
  Scatter -> case name gh of
              Blinky -> (25,30)
              Pinky  -> (2,30)
              Inky   -> (25,0)
              Clyde  -> (2,0)
  Chasing -> case name gh of
              Blinky -> targetTileBlinky pTile
              Pinky -> targetTilePinky gs pTile
              Inky -> targetTileInky gs pTile
              Clyde -> targetTileClyde gh pTile
  where
    pTile = posToTile pPos        --player tile
    pPos = getPosition (player gs) --player position

--targetTileBlinky takes player tile and returns target tile:
targetTileBlinky :: (Int, Int) -> (Int, Int)
targetTileBlinky pt = pt --tile the player is on

targetTilePinky :: GameState -> (Int, Int) -> (Int, Int)
targetTilePinky gs (pX,pY) = targTile
  where
    targTile = case P.direction (player gs) of --tile 4 steps ahead of pacman
                  Up    -> (pX, pY + 4)
                  Down  -> (pX, pY - 4)
                  Left  -> (pX - 4, pY)
                  Right -> (pX + 4, pY)

--tricky one: target tile dependent on Blinky's position
targetTileInky :: GameState -> (Int, Int) -> (Int, Int)
targetTileInky gs (pX, pY) = targTile
  where
    --target tile: intermediate tile plus the tile "vector"
    --             from blinky to this tile:
    targTile = (iX + vX, iY + vY)

    --tile "vector" from blinky to intermediate tile:
    (vX, vY) = (iX - bX, iY - bY)

    (bX, bY) = posToTile bPos --blinky tile
    bPos = getPosition (head (ghosts gs)) --blinky position ("head" because blinky is the first of defaultGhosts)

    (iX, iY) = case P.direction (player gs) of --"intermediate" tile: the tile 2 tiles ahead of pac man
                  Up    -> (pX, pY + 2)
                  Down  -> (pX, pY - 2)
                  Left  -> (pX - 2, pY)
                  Right -> (pX + 2, pY)

targetTileClyde :: Ghost -> (Int, Int) -> (Int, Int)
targetTileClyde gh pt
  | dist < 64 = (2, 0) --scatter tile if clyde is closer than 8 tiles to pacman
  | otherwise = pt --player tile if clyde is further away than 8 tiles
  where
    dist = sqTileDist pt cTile --distance from clyde to pac-man

    cTile = posToTile cPos --clyde tile
    cPos = getPosition gh --clyde position


--Convert float coordinate to tile (int) coordinate
posToTile :: Position -> (Int, Int)
posToTile (x, y) = (round x, round y)

--Compute squared tile distance between two tiles
sqTileDist :: (Int, Int) -> (Int, Int) -> Int
sqTileDist (p1X, p1Y) (p2X, p2Y)
  = xDif*xDif + yDif*yDif
  where
    xDif = p1X - p2X
    yDif = p1Y - p2Y


--TODO: ESCAPE-FUNCTIE VOOR EEN GHOST: BEGIN MET NAAR BOVEN BEWEGEN ()
-- VERDER: ALS GHOST GEGETEN WORDT WEER LATEN TERUGKEREN NAAR SPAWN MET DIRECTION = STOP
--  EN NA KORTE TIJD WEER DE ESCAPE-FUNCTIE AANROEPEN














--END

module Model.GhostAI where

import Controller.MovementController
import Data.Fixed (mod')
import Data.Maybe (Maybe (..), catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Model.Game
import Model.Ghosts as G
import Model.Level
import Model.Movement
import Model.Player as P
import Numeric (showFFloat)
import Prelude hiding (Down, Left, Right, Up)

-------------------------------------------------------------------------------

makeGhostsMove :: GameState -> GameState
makeGhostsMove gs = gs {ghosts = map (makeGhostMoveEv gs) (ghosts gs)}

-------------------------------------------------------------------------------
--Check ghost isEaten:

makeGhostMoveEv :: GameState -> Ghost -> Ghost
makeGhostMoveEv gs ghst
  | not $ isEaten ghst = makeGhostMove gs ghst
  | not $ goesBack ghst = (makeGhostMove gs ghst) {G.position = gTilePos, G.speed = 1 / 2, goesBack = True} --start to bring ghost back to spawn
  | gTile == spawnPoint ghst = (makeGhostMove gs ghst) {G.position = gTilePos, eatenState = NotEaten, G.speed = 0.125, G.direction = Up, opDirection = Down, nextDirection = nextDir, goesBack = False, wellPositionedTarget = False}
  | gTile /= targetpoint || wellPositionedTarget ghst = makeGhostMove gs ghst --gaat op target tile vlakbij spawn af.
  | otherwise = (makeGhostMove gs ghst) {G.position = gTilePos, G.direction = Down, opDirection = Up, wellPositionedTarget = True}
  where
    nextDir = case name ghst of
      Blinky -> Left
      Pinky -> Left
      Inky -> Right
      Clyde -> Right

    targetpoint = case name ghst of
      Blinky -> (13, 19)
      Pinky -> (13, 19)
      Inky -> (14, 19)
      Clyde -> (14, 19)

    gTilePos = ghostTilePosition ghst
    gTile = getPosition ghst

makeGhostMove :: GameState -> Ghost -> Ghost
makeGhostMove gs ghst
  | onIntersectionTile gs ghst --hiervoor: check case ghostMode (frightened : fMovedGhost ofzo; otherwise: check on intersection tile + rest)
    =
    fromMaybe (fromMaybe movedir movedGhost) bufMovedGhost -- Fallback on moveDir if ghost cannot mopve to bufDirection
  | isJust nextintersect = movedGhostWithNextDir -- voor snelheid: (checkedAtTile ghst /= gTile) &&  -- if we didn't check the next tile already, only then we check intersection
  | isNothing movedGhost = movedir
  | entersTunnel ghst gTile && not (isInTunnel ghst) = slowGhostDownTunnel (fromJust movedGhost) {isInTunnel = True} -- G.position = gTilePos,  {checkedAtTile = gTile} -- if ghost can only walk further
  | leavesTunnel ghst gTile && isInTunnel ghst = speedGhostUpTunnel (fromJust movedGhost) {isInTunnel = False} -- G.position = gTilePos,
  | otherwise = fromJust movedGhost
  where
    movedir = checkMoveDirs gs ghst --if ghost meets a wall (which is no intersection)
    nextintersect = checkMoveToIntersection gs ghst

    movedGhost = makeDirectionMoveGhost gs ghst (G.direction ghst)
    movedGhostWithNextDir = (fromJust movedGhost) {nextDirection = nextdir}
    --movedGhostOpp = makeDirectionMoveGhost gs ghst (opDirection ghst) --om te checken of hij de intersections "ziet"
    bufMovedGhost = makeDirectionMoveGhost gs ghst (nextDirection ghst)
    --nextdir returns next direction of ghost at the following intersection:
    nextdir = chooseAtIntersection gs ghst (fromJust nextintersect)

    gTilePos = ghostTilePosition ghst
    gTile@(gtX, gtY) = intPosition $ getPosition ghst

--------------------------------------------------------------------------------

--checkMoveDirs is called if ghost meets a wall
checkMoveDirs :: GameState -> Ghost -> Ghost
checkMoveDirs gs gh =
  if gTile `elem` ([(u, 15) | u <- [13 .. 16]])
    then fromJust (makeDirectionMoveGhost gs gh (opDirection gh))
    else case length possiblemoves of
      0 -> movedGhost {G.position = gPos, G.direction = opp (G.direction gh), opDirection = G.direction gh} -- turnAround {G.position = gTilePos, nextDirection = G.direction turnAround} --goes back if there's nothing else (dead end)
      _ -> pickFavDir {nextDirection = G.direction pickFavDir}
  where
    gTile = intPosition gPos
    gPos = getPosition gh
    gTilePos = ghostTilePosition gh

    turnAround = fromJust (makeDirectionMoveGhost gs gh (opDirection gh))
    pickFavDir = head possiblemoves

    possiblemoves =
      catMaybes
        [ makeDirectionMoveGhost gs gh x
          | x <- filter (\x -> x /= opDirection gh) [Up, Left, Down, Right] -- && x /= G.direction gh
        ]
    -- u = filter (\x -> x /= opDirection gh) [Up, Left, Down, Right] -- && x /= G.direction gh
    movedGhost = move gh (opDirection gh) (layoutSize . layout $ lvl)
    lvl = level gs

makeDirectionMoveGhost :: GameState -> Ghost -> Direction -> Maybe Ghost
makeDirectionMoveGhost gs ghst dir
  | canMoveInDir && isValidMovePosition = Just updatedGhost
  | otherwise = Nothing
  where
    canMoveInDir = canMakeMoveToDirGhost gs ghst dir lvl
    isValidMovePosition = isValidGhostPosition lvl movedGhost

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
    isValid = isValidGhostPosition lvl (moveFull gh dir)
    (x, y) = getPosition gh

-- | Checks if the ghost is in a valid position on the level
isValidGhostPosition :: Level -> Ghost -> Bool
isValidGhostPosition lvl gh = isValidMovablePosition (`elem` validTiles) lvl gh
  where
    validTiles = case G.direction gh of -- can this not be simplified to just if eaten?
      Up -> [Floor, GhostDoor Open, GhostDoor Closed]
      _ ->
        if isEaten gh
          then [Floor, GhostDoor Open, GhostDoor Closed]
          else [Floor]

--------------------------------------------------------------------------------

checkMoveToIntersection :: GameState -> Ghost -> Maybe (Int, Int)
checkMoveToIntersection gs gh
  | nextpoint `elem` intersections = Just nextpoint
  | otherwise = Nothing
  where
    (gX, gY) = intPosition $ getPosition gh --ghost tile
    nextpoint = case G.direction gh of
      Up -> (gX, gY + 1)
      Down -> (gX, gY - 1)
      Left -> (gX - 1, gY)
      Right -> (gX + 1, gY)
      Stop -> (gX, gY)
    intersections =
      [ (a, b)
        | (a, b) <- levelIntersections . level $ gs,
          (a, b) `notElem` ([(c, d) | c <- [11 .. 16], d <- [15 .. 17]]),
          not (elem (a, b) [(12, 7), (15, 7), (12, 19), (15, 19)] && G.direction gh /= Down && ghostMode gs /= Frightened)
      ]

-- ,not (elem (a,b) [(12,7), (15,7), (12,19), (15,19)] && G.direction gh /= Down )
-- above: if the ghost happens to be going to one of the non-intersection tiles (4 of them)
-- while moving up left or right, it will not see it as an intersection (moving down, it will)

onIntersectionTile :: GameState -> Ghost -> Bool
onIntersectionTile gs gh = gTile `elem` intersections
  where
    gTile = intPosition $ getPosition gh

    intersections =
      [ (a, b)
        | (a, b) <- levelIntersections . level $ gs,
          (a, b) `notElem` ([(c, d) | c <- [11 .. 16], d <- [15 .. 17]]),
          not (elem (a, b) [(12, 7), (15, 7), (12, 19), (15, 19)] && elem (G.direction gh) [Left, Right] && ghostMode gs /= Frightened)
      ]

-- ,not (elem (a,b) [(12,7), (15,7), (12,19), (15,19)] && elem (G.direction gh) [Left, Right] )
-- in case the ghost moves Up out of spawn, it won't "see" the intersection in advance, but once it's on,
--  moves to its predefined (from spawn) nextDirection.

chooseAtIntersection :: GameState -> Ghost -> (Int, Int) -> Direction
chooseAtIntersection gs gh (iX, iY) = bestDir
  where
    bestDir = snd (minimum distsDirs)

    distsDirs = zip distances posDirections

    posDirections = [d | d <- allDirections, d /= opDir, isFloor d]
    distances = [sqTileDist (tileAfter d) target | d <- allDirections, d /= opDir, isFloor d]

    isFloor dir = tileAtW (level gs) (tileAfter dir) `elem` [Floor, GhostDoor Open]

    tileAfter dir = case dir of --tiles to go to from intersection
      Up -> (iX, iY + 1)
      Down -> (iX, iY - 1)
      Left -> (iX - 1, iY)
      Right -> (iX + 1, iY)

    allDirections = [Up, Down, Left, Right]
    opDir = opDirection gh --opposite direction of ghost
    target = targetTileGhost gs gh --target tile of ghost
    --------------------------------------------------------------------------------

entersTunnel :: Ghost -> (Int, Int) -> Bool
entersTunnel gh gt = (gt == (5, 16) && G.direction gh == Left) || (gt == (22, 16) && G.direction gh == Right)

leavesTunnel :: Ghost -> (Int, Int) -> Bool
leavesTunnel gh gt = (gt == (4, 16) && G.direction gh == Right) || (gt == (23, 16) && G.direction gh == Left)

--------------------------------------------------------------------------------
--Scatter/Chase Mode:
targetTileGhost :: GameState -> Ghost -> (Int, Int)
targetTileGhost gs gh
  | isEaten gh = case name gh of
    Blinky -> (13, 19)
    Pinky -> (13, 19)
    Inky -> (14, 19)
    Clyde -> (14, 19)
  | otherwise = case ghostMode gs of
    Scatter -> case name gh of
      Blinky -> (27, 26)
      Pinky -> (0, 26)
      Inky -> (25, 0)
      Clyde -> (2, 0)
    Chasing -> case name gh of
      Blinky -> targetTileBlinky pTile
      Pinky -> targetTilePinky gs pTile
      Inky -> targetTileInky gs gh pTile
      Clyde -> targetTileClyde gh pTile
    Frightened -> fst $ randomTile (ranGen gs) gs
  where
    pTile = intPosition . getPosition $ player gs --player tile position

--targetTileBlinky takes player tile and returns target tile:
targetTileBlinky :: (Int, Int) -> (Int, Int)
targetTileBlinky pt = pt --tile the player is on

--targetTilePinky takes gamestate and player tile and returns target tile:
targetTilePinky :: GameState -> (Int, Int) -> (Int, Int)
targetTilePinky gs (pX, pY) = targTile
  where
    targTile = case P.direction (player gs) of --tile 4 steps ahead of pacman
      Up -> (pX, pY + 4)
      Down -> (pX, pY - 4)
      Left -> (pX - 4, pY)
      Right -> (pX + 4, pY)
      Stop -> case P.bufDirection (player gs) of
        Up -> (pX, pY + 4)
        Down -> (pX, pY - 4)
        Left -> (pX - 4, pY)
        Right -> (pX + 4, pY)
        Stop -> (pX, pY)

--tricky one: target tile dependent on Blinky's position
targetTileInky :: GameState -> Ghost -> (Int, Int) -> (Int, Int)
targetTileInky gs gh pt@(pX, pY)
  | dist > 16 = targTile
  | otherwise = pt
  where
    dist = sqTileDist pt iTile

    iTile = intPosition $ getPosition gh --inky tile position

    --target tile: intermediate tile plus the tile "vector"
    --             from blinky to this tile:
    targTile = (iX + vX, iY + vY)

    --tile "vector" from blinky to intermediate tile:
    (vX, vY) = (iX - bX, iY - bY)

    (bX, bY) = intPosition . getPosition . head $ filter (\g -> name g == Inky) (ghosts gs)

    --blinky tile
    bPos = getPosition (head (ghosts gs)) --blinky position ("head" because blinky is the first of defaultGhosts)
    (iX, iY) = case P.direction (player gs) of --"intermediate" tile: the tile 2 tiles ahead of pac man
      Up -> (pX, pY + 2)
      Down -> (pX, pY - 2)
      Left -> (pX - 2, pY)
      Right -> (pX + 2, pY)
      Stop -> case P.bufDirection (player gs) of
        Up -> (pX, pY + 2)
        Down -> (pX, pY - 2)
        Left -> (pX - 2, pY)
        Right -> (pX + 2, pY)
        Stop -> (pX, pY)

targetTileClyde :: Ghost -> (Int, Int) -> (Int, Int)
targetTileClyde gh pt
  | dist < 64 = (2, 0) --scatter tile if clyde is closer than 8 tiles to pacman
  | otherwise = pt --player tile if clyde is further away than 8 tiles
  where
    dist = sqTileDist pt cTile --distance from clyde to pac-man
    cTile = intPosition $ getPosition gh --clyde tile position

--Compute squared tile distance between two tiles
sqTileDist :: (Int, Int) -> (Int, Int) -> Int
sqTileDist (p1X, p1Y) (p2X, p2Y) =
  xDif * xDif + yDif * yDif
  where
    xDif = p1X - p2X
    yDif = p1Y - p2Y

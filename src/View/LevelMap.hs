module View.LevelMap where

import Data.List (elemIndex, find)
import Data.List.Index (imap)
import Data.Maybe (fromJust, isJust)
import Model.Level
import Model.Dijkstra
import Model.Movement

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Rotation direction of a tile
data Rotation = N | E | S | W deriving (Eq, Show, Enum)

-- | Indicates if a tile's texture should be rendered flipped
data Mirrored = Mirrored | NotMirrored deriving (Eq, Show, Enum)

-- | Texture types of tiles in a level
data TextureTile
  = None
  | Straight Rotation
  | StraightSingle Rotation
  | Corner Rotation
  | CornerSingle Rotation
  | CornerSingleToDouble Rotation Mirrored
  | CrossSectionSingle
  | CrossSectionFishShaped Rotation
  | Tjunction Rotation
  | TjunctionSingle Rotation
  | SurroundedWall
  | EndingSingle Rotation
  | GhostDoorStraight Rotation
  | GhostDoorCorner Rotation
  | Dev --Fallback type
  deriving (Show, Eq)

-- | Type alias for a Layout with the reachability of a tile and its surrounding tiles
type WallNeighbors = Layout Reachability

-- | Data type defining if a tile is reachable by a player
data Reachability = Reachable | Unreachable | Door deriving (Show, Eq)

data Pattern = Pattern
  { pattern :: WallNeighbors,
    rotation :: Rotation,
    mirrored :: Mirrored
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | Converts a level to a layout of texture tiles
-- | The layout can be interpreted for specific texture rendering
convertLevel :: Level -> Layout TextureTile
convertLevel lvl@Level {layout = Layout xss} = Layout (imap (\y -> imap (\x -> convert (x, y))) xss)
  where
    convert :: (Int, Int) -> Tile -> TextureTile
    convert pos tile = case tile of
      Floor -> None
      _ -> generateTextureTile pos

    generateTextureTile :: (Int, Int) -> TextureTile
    generateTextureTile (x, y)
      | isJust mStraight = Straight (rotation $ fromJust mStraight)
      | isJust mStraightSingle = StraightSingle (rotation $ fromJust mStraightSingle)
      | isJust mCorner = Corner (rotation $ fromJust mCorner)
      | isJust mCornerSingle = CornerSingle (rotation $ fromJust mCornerSingle)
      | isJust mCornerSingleToDouble = CornerSingleToDouble (rotation $ fromJust mCornerSingleToDouble) (mirrored $ fromJust mCornerSingleToDouble)
      | isJust mCrossection = CrossSectionSingle
      | isJust mFishCrosSection = CrossSectionFishShaped (rotation $ fromJust mFishCrosSection)
      | isJust mTJunction = Tjunction (rotation $ fromJust mTJunction)
      | isJust mTJunctionSingle = TjunctionSingle (rotation $ fromJust mTJunctionSingle)
      | isJust mCenter = SurroundedWall
      | isJust mEnd = EndingSingle (rotation $ fromJust mEnd)
      | isJust mStraightDoor = GhostDoorStraight (rotation $ fromJust mStraightDoor)
      | isJust mCornerDoor = GhostDoorCorner (rotation $ fromJust mCornerDoor)
      | otherwise = Dev
      where
        matrix = generateWallNeighbors (x, y)

        mCenter = isSurroundedWallTextureTile matrix
        mStraight = isStraightTextureTile matrix
        mStraightSingle = isStraightSingleTextureTile matrix
        mCorner = isCornerTextureTile matrix
        mCornerSingle = isCornerSingleTextureTile matrix
        mCornerSingleToDouble = isCornerSingleToDoubleFloorsTextureTile matrix
        mTJunction = isTJunctionTextureTile matrix
        mTJunctionSingle = isTJunctionSingleTextureTile matrix
        mCrossection = isCrossSectionTextureTile matrix
        mFishCrosSection = isCrossSectionFishShapedFloorsTextureTile matrix
        mEnd = isEndingSingleTextureTile matrix
        mStraightDoor = isStraightDoorTextureTile matrix
        mCornerDoor = isCornerSingleDoorTextureTile matrix

    generateWallNeighbors :: (Int, Int) -> WallNeighbors
    generateWallNeighbors (x, y) =
      Layout
        [ [ul, u, ur],
          [l, getReachability $ tileAtW lvl (x, y), r],
          [dl, d, dr]
        ]
      where
        (w, h) = layoutSize . layout $ lvl

        getReachability :: Tile -> Reachability
        getReachability (GhostDoor _) = Door
        getReachability _ = Unreachable

        u = isUnreachable (x, y + 1)
        d = isUnreachable (x, y - 1)
        l = isUnreachable (x - 1, y)
        r = isUnreachable (x + 1, y)

        ul = isUnreachable (x -1, y + 1)
        ur = isUnreachable (x + 1, y + 1)
        dl = isUnreachable (x - 1, y -1)
        dr = isUnreachable (x + 1, y - 1)

        isUnreachable (x, y)
          | x < 0 = Reachable
          | y < 0 = Reachable
          | x >= w = Reachable
          | y >= h = Reachable
          | tileAtW lvl (x, y) `elem` [Wall, GhostDoor Open, GhostDoor Closed] = Unreachable
          | otherwise = Reachable

-------------------------------------------------------------------------------
-- Pattern helper functions
-------------------------------------------------------------------------------

mkStaticPattern :: WallNeighbors -> Pattern
mkStaticPattern wn = Pattern wn N NotMirrored

mkPatternSet :: WallNeighbors -> Int -> Mirrored -> [Pattern]
mkPatternSet wn amntRotations m
  | amntRotations > 4 = error "amntRotations cannot be larger than 4"
  | amntRotations < 0 = error "amntRotations cannot be smaller than 0"
  | otherwise =
    let func = (\n (nextPattern, acc) -> ((rotR nextPattern), Pattern nextPattern (toEnum n) m : acc))
     in snd $ foldr func (wn, []) [0 .. amntRotations]

--------------------
rotatedSet :: WallNeighbors -> [Pattern]
rotatedSet wn = mkPatternSet wn 4 NotMirrored

rotatedMirroredSet :: WallNeighbors -> [Pattern]
rotatedMirroredSet wn = mkPatternSet (mirrorH wn) 4 Mirrored

halfRotatedSet :: WallNeighbors -> [Pattern]
halfRotatedSet wn = mkPatternSet wn 2 NotMirrored

halfRotatedMirroredSet :: WallNeighbors -> [Pattern]
halfRotatedMirroredSet wn = mkPatternSet (mirrorH wn) 2 Mirrored

--------------------
completeSet :: WallNeighbors -> [Pattern]
completeSet wn = rotatedSet wn ++ rotatedMirroredSet wn

completeHalfSet :: WallNeighbors -> [Pattern]
completeHalfSet wn = halfRotatedSet wn ++ halfRotatedMirroredSet wn

--------------------
match :: WallNeighbors -> [Pattern] -> Maybe Pattern
match wn = find (\p -> pattern p == wn)

------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

-- | Creates a list consisting of n different (90 deg) rotations of WallNeighbors
-- | n == 0 = []
-- | n == 1 = [id]
-- | Limited at 4 rotations, because that is a full 360 deg rotation
rotRNTimes :: Int -> WallNeighbors -> [WallNeighbors]
rotRNTimes 0 _ = []
rotRNTimes n matrix
  | n < 0 = []
  | n > 4 = rotRNTimes 4 matrix
  | otherwise = matrix : rotRNTimes (n -1) (rotR matrix)

------------------------------------------------------------------------
-- Neigboring Tile Patterns
------------------------------------------------------------------------
{-
  ◯ == Reachable
  ▊ == Unreachable
  ■  == Ghoost Door
-}
-----------------------
-- CenterWall
-----------------------
isSurroundedWallTextureTile :: WallNeighbors -> Maybe Pattern
isSurroundedWallTextureTile wn = match wn [surroundedWallFloors]

surroundedWallFloors =
  mkStaticPattern $
    Layout
      [ [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable]
      ]

-----------------------
-- Straight
-----------------------
{-
  Straight  Cases:  (Bothsides A's) Rotations (4)
  ◯◯◯
  ▊▊▊
  ▊▊▊

  Beside Corner Cases: Rotations (4) Mirrored (2)
  ▊◯◯
  ▊▊▊
  ▊▊▊
-}

isStraightTextureTile :: WallNeighbors -> Maybe Pattern
isStraightTextureTile wn = match wn (fullSideFloors ++ cornerAndSideFloors)

fullSideFloors =
  rotatedSet $
    Layout
      [ [Reachable, Reachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable]
      ]

cornerAndSideFloors =
  completeSet $
    Layout
      [ [Unreachable, Reachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable]
      ]

-----------------------
-- Straight Single
-----------------------
{-
  StraightSingle  Cases:  (Bothsides A's) Rotations (2)
  ◯◯◯
  ▊▊▊
  ◯◯◯

  StraightSingle case Oposite Beside Corner Cases: Rotations (2) Mirrored (2)
  ◯◯▊
  ▊▊▊
  ▊◯◯

  StraightSingle Case Same Side CornerAndSide Cases: Rotations (4)
  ▊◯▊
  ▊▊▊
  ▊◯◯

 StraightSingle Case L-Shape Cases: Rotations (4) Mirrored (2)
  ◯◯▊
  ▊▊▊
  ◯◯◯

  StraightSingle Case Helmet Shape Middle Cases: Rotations (4)
  ◯◯◯
  ▊▊▊
  ▊◯▊

  StraightSingle Case h-Shape Cases: Rotations (4) Mirrored (2)
  ◯◯▊
  ▊▊▊
  ▊◯▊

  StraightSingle Case H-Shape Cases: Rotations (4)
  ▊◯▊
  ▊▊▊
  ▊◯▊

-}

isStraightSingleTextureTile :: WallNeighbors -> Maybe Pattern
isStraightSingleTextureTile wn =
  match wn $
    fullBothSidesFloors
      ++ opositeCornerAndSideFloors
      ++ sameSideCornerAndSideFloors
      ++ straightLShapeFloors
      ++ straightHelmetShapeFloors
      ++ straighthShapeFloors
      ++ straightHShapeFloors

fullBothSidesFloors =
  halfRotatedSet $
    Layout
      [ [Reachable, Reachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

opositeCornerAndSideFloors =
  completeHalfSet $
    Layout
      [ [Reachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Reachable]
      ]

sameSideCornerAndSideFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Reachable, Unreachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Reachable, Reachable]
      ]

straightLShapeFloors =
  completeSet $
    Layout
      [ [Reachable, Reachable, Unreachable],
        [Unreachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

straightHelmetShapeFloors =
  rotatedSet $
    Layout
      [ [Reachable, Reachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

straighthShapeFloors =
  completeSet $
    Layout
      [ [Reachable, Reachable, Unreachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

straightHShapeFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Reachable, Unreachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

-----------------------
-- Corner
-----------------------
{-
  Corner Cases:
  ▊▊◯
  ▊▊▊
  ▊▊▊

  BigFloorCorner Cases: Rotations (4)
  ◯▊▊
  ◯▊▊
  ◯◯◯

  Small FloorCorner Cases: Rotations (4)
  ▊▊▊
  ◯▊▊
  ◯◯▊

  L-FloorCorner Cases: Rotations (4) Mirrored (2)
  ◯▊▊
  ◯▊▊
  ◯◯▊

-}

isCornerTextureTile :: WallNeighbors -> Maybe Pattern
isCornerTextureTile wn = match wn (cornerFloors ++ bigCornerFloors ++ smallCornerFloors ++ lShapedCornerFloors)

cornerFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable]
      ]

bigCornerFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

smallCornerFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Unreachable],
        [Reachable, Reachable, Unreachable]
      ]

lShapedCornerFloors =
  completeSet $
    Layout
      [ [Reachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Unreachable],
        [Reachable, Reachable, Unreachable]
      ]

-----------------------
-- CornerSingle
-----------------------
{-
  CornerSingle Cases: Rotations (4)
  ◯▊◯
  ◯▊▊
  ◯◯◯

  CornerTwoSeparateFloors Cases: Rotations (4) (Special case for single corner that closes diaonally)
  ◯▊◯
  ◯▊▊
  ▊◯◯

  CornerAndOpositeSmallCorner Cases: Rotations (4) (Special case of single to maybe multiple corners)
  ▊▊▊
  ◯▊▊
  ◯◯▊

  -- Corner But has underconnection to single Cases: Rotations (4)
  --   ◯▊▊
  --   ◯▊▊
  --   ▊▊◯
-}

isCornerSingleTextureTile :: WallNeighbors -> Maybe Pattern
isCornerSingleTextureTile wn =
  match wn $
    cornerSingleFloors
      ++ cornerSingleAndDiagonalFloors
      ++ cornerSingleDiagonalFloors

cornerSingleFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Reachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

cornerSingleAndDiagonalFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Reachable, Unreachable, Unreachable],
        [Unreachable, Reachable, Reachable]
      ]

-- Special Case :: CornerSingle With diagonal Connection
cornerSingleDiagonalFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Unreachable],
        [Reachable, Reachable, Unreachable]
      ]

-----------------------
-- CornerSingleToDouble
-----------------------
{-
  CornerSingleToDouble Cases: Rotations (4) Mirrored (2)
  ▊▊◯
  ▊▊▊
  ◯◯◯
-}

isCornerSingleToDoubleFloorsTextureTile :: WallNeighbors -> Maybe Pattern
isCornerSingleToDoubleFloorsTextureTile wn = match wn singleToDoubleCornerFloors

singleToDoubleCornerFloors =
  completeSet $
    Layout
      [ [Unreachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

-----------------------
-- T-Junction
-----------------------

{-
  T-Junction Cases: Rotations (4)
  ◯▊◯
  ▊▊▊
  ▊▊▊
-}

isTJunctionTextureTile :: WallNeighbors -> Maybe Pattern
isTJunctionTextureTile wn = match wn tJunctions

tJunctions =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Unreachable]
      ]

-----------------------
-- T-Junction Single
-----------------------
{-
  T-JunctionSingle Cases: Rotations (4)
  ◯▊◯
  ▊▊▊
  ◯◯◯
-}

isTJunctionSingleTextureTile :: WallNeighbors -> Maybe Pattern
isTJunctionSingleTextureTile wn = match wn tJunctionSingles

tJunctionSingles =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

-----------------------
-- CrossSection
-----------------------
{-
  CrossSectionSingle:
  ◯▊◯
  ▊▊▊
  ◯▊◯
-}

isCrossSectionTextureTile :: WallNeighbors -> Maybe Pattern
isCrossSectionTextureTile wn = match wn [crossSections]

crossSections =
  mkStaticPattern $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Reachable, Unreachable, Reachable]
      ]

-----------------------
-- CrossSectionFishShaped
-----------------------
{-
  CrossSectionFishShaped Cases: Rotations (4)
  ◯▊◯
  ▊▊▊
  ▊▊◯
-}

isCrossSectionFishShapedFloorsTextureTile :: WallNeighbors -> Maybe Pattern
isCrossSectionFishShapedFloorsTextureTile wn = match wn crossSectionFishShapedFloors

crossSectionFishShapedFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Unreachable, Unreachable, Unreachable],
        [Unreachable, Unreachable, Reachable]
      ]

-----------------------
-- EndingSingle
-----------------------
{-
  EndingSingle Cases: Rotations (4) (looks like U-junction of floors)
  ◯▊◯
  ◯▊◯
  ◯◯◯

  HelmetEndingSingle Cases: Rotations (4) (looks like small U-junction of floors)
  ▊▊▊
  ◯▊◯
  ◯◯◯

  EndingSingle Cases: Rotations (4) Mirror (2) (looks like J-junction of floors)
  ◯▊▊
  ◯▊◯
  ◯◯◯
-}

isEndingSingleTextureTile :: WallNeighbors -> Maybe Pattern
isEndingSingleTextureTile wn = match wn $ fullEndingSingles
  ++ shortEndingSingles
  ++ partialEndingSingles

fullEndingSingles = rotatedSet $ Layout
  [ [Reachable, Unreachable, Reachable],
    [Reachable, Unreachable, Reachable],
    [Reachable, Reachable, Reachable]
  ]

shortEndingSingles =rotatedSet $ Layout
  [ [Unreachable, Unreachable, Unreachable],
    [Reachable, Unreachable, Reachable],
    [Reachable, Reachable, Reachable]
  ]

partialEndingSingles = rotatedSet $ Layout
  [ [Reachable, Unreachable, Unreachable],
    [Reachable, Unreachable, Reachable],
    [Reachable, Reachable, Reachable]
  ]

-- TODO
-----------------------
-- GhostDoorStraight
-----------------------
{-
  GhostDoorStraight Cases:
  ◯▊
-}
{-
  StraightSingleDoor  Cases:  (Bothsides A's) Rotations (2)
  ◯◯◯
  ▊■▊
  ◯◯◯

  StraightSingleDoor case Oposite Beside Corner Cases: Rotations (2) Mirrored (2)
  ◯◯▊
  ▊■▊
  ▊◯◯

  StraightSingleDoor Case Same Side CornerAndSide Cases: Rotations (4)
  ▊◯▊
  ▊■▊
  ▊◯◯

 StraightSingleDoor Case L-Shape Cases: Rotations (4) Mirrored (2)
  ◯◯▊
  ▊■▊
  ◯◯◯

  StraightSingleDoor Case Helmet Shape Middle Cases: Rotations (4)
  ◯◯◯
  ▊■▊
  ▊◯▊

  StraightSingleDoor Case h-Shape Cases: Rotations (4) Mirrored (2)
  ◯◯▊
  ▊■▊
  ▊◯▊

  StraightSingleDoor Case H-Shape Cases: Rotations (4)
  ▊◯▊
  ▊■▊
  ▊◯▊

-}

isStraightDoorTextureTile :: WallNeighbors -> Maybe Pattern
isStraightDoorTextureTile wn =
  match wn $
    fullBothSidesDoorFloors
      ++ opositeCornerAndSideDoorFloors
      ++ sameSideCornerAndSideDoorFloors
      ++ straightLShapeDoorFloors
      ++ straightHelmetShapeDoorFloors
      ++ straighthShapeDoorFloors
      ++ straightHShapeDoorFloors

fullBothSidesDoorFloors =
  halfRotatedSet $
    Layout
      [ [Reachable, Reachable, Reachable],
        [Unreachable, Door, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

opositeCornerAndSideDoorFloors =
  completeHalfSet $
    Layout
      [ [Reachable, Unreachable, Unreachable],
        [Reachable, Door, Reachable],
        [Unreachable, Unreachable, Reachable]
      ]

sameSideCornerAndSideDoorFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Reachable, Unreachable],
        [Unreachable, Door, Unreachable],
        [Unreachable, Reachable, Reachable]
      ]

straightLShapeDoorFloors =
  completeSet $
    Layout
      [ [Reachable, Reachable, Unreachable],
        [Unreachable, Door, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

straightHelmetShapeDoorFloors =
  rotatedSet $
    Layout
      [ [Reachable, Reachable, Reachable],
        [Unreachable, Door, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

straighthShapeDoorFloors =
  completeSet $
    Layout
      [ [Reachable, Reachable, Unreachable],
        [Unreachable, Door, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

straightHShapeDoorFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Reachable, Unreachable],
        [Unreachable, Door, Unreachable],
        [Unreachable, Reachable, Unreachable]
      ]

-----------------------
-- GhostDoorCorner
-----------------------
{-
  CornerSingleDoor Cases: Rotations (4)
  ◯▊◯
  ◯■ ▊
  ◯◯◯

  CornerTwoSeparateDoorFloors Cases: Rotations (4) (Special case for single corner that closes diaonally)
  ◯▊◯
  ◯■▊
  ▊◯◯

  CornerAndOpositeSmallCornerDoor Cases: Rotations (4) (Special case of single to maybe multiple corners)
  ▊▊▊
  ◯■▊
  ◯◯▊

  -- CornerDoor But has underconnection to single Cases: Rotations (4)
  --   ◯▊▊
  --   ◯■▊
  --   ▊▊◯
-}

isCornerSingleDoorTextureTile :: WallNeighbors -> Maybe Pattern
isCornerSingleDoorTextureTile wn =
  match wn $
    cornerSingleDoorFloors
      ++ cornerSingleAndDiagonalDoorFloors
      ++ cornerSingleDiagonalDoorFloors

cornerSingleDoorFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Reachable, Door, Unreachable],
        [Reachable, Reachable, Reachable]
      ]

cornerSingleAndDiagonalDoorFloors =
  rotatedSet $
    Layout
      [ [Reachable, Unreachable, Reachable],
        [Reachable, Door, Unreachable],
        [Unreachable, Reachable, Reachable]
      ]

-- Special Case :: CornerSingle With diagonal Connection
cornerSingleDiagonalDoorFloors =
  rotatedSet $
    Layout
      [ [Unreachable, Unreachable, Unreachable],
        [Reachable, Door, Unreachable],
        [Reachable, Reachable, Unreachable]
      ]

module View.LevelMap where

import Data.List.Index (imap)
import Model.Level

data TextureTile
  = None
  | Straight
  | StraightSingle
  | Corner
  | CornerSingle
  | Tjunction
  | TjunctionSingle
  | CrossSectionSingle
  | SurroundedWall
  | EndingSingle
  | Dev --Fallback type
  deriving (Show, Eq)

type WallNeighbors = Layout TileType

-- TODO: Maybe overagaan naar __ data TileType = Wall | Neutral/None (voor floor, OOB en unreachable floors)
data TileType
  = Root
  | FloorTile
  | WallTile
  | OutOfBounds -- Meaning outside of map OR unreachable floor tile
  deriving (Show)

instance Eq TileType where
  Root == Root = True
  FloorTile == FloorTile = True
  WallTile == WallTile = True
  OutOfBounds == OutOfBounds = True
  -- Special cases for OutOfBounds and FloorTile
  FloorTile == OutOfBounds = True
  OutOfBounds == FloorTile = True
  _ == _ = False

convertLevel :: Level -> Layout TextureTile
convertLevel lvl@Level {layout = Layout xss} = Layout (imap (\y -> imap (\x -> convert (x, y))) xss)
  where
    convert :: (Int, Int) -> Tile -> TextureTile
    convert pos tile = case tile of
      Wall -> calculateConvert pos
      _ -> None

    calculateConvert :: (Int, Int) -> TextureTile
    calculateConvert (x, y)
      | isStraightTextureTile matrix = Straight
      | isStraightSingleTextureTile matrix = StraightSingle
      | isCornerTextureTile matrix = Corner
      | isCornerSingleTextureTile matrix = CornerSingle
      | isTJunctionSingleTextureTile matrix = TjunctionSingle
      | isTJunctionTextureTile matrix = Tjunction
      | isCrossSectionTextureTile matrix = CrossSectionSingle
      | isSurroundedWallTextureTile matrix = SurroundedWall
      | isEndingSingleTextureTile matrix = EndingSingle
      | otherwise = Dev
      where
        matrix = generateWallNeighbors (x, y)

    generateWallNeighbors :: (Int, Int) -> WallNeighbors
    generateWallNeighbors (x, y) =
      Layout
        [ [ul, u, ur],
          [l, Root, r],
          [dl, d, dr]
        ]
      where
        (w, h) = layoutSize . layout $ lvl

        u = isWallTile (x, y + 1)
        d = isWallTile (x, y - 1)
        l = isWallTile (x - 1, y)
        r = isWallTile (x + 1, y)

        ul = isWallTile (x -1, y + 1)
        ur = isWallTile (x + 1, y + 1)
        dl = isWallTile (x - 1, y -1)
        dr = isWallTile (x + 1, y - 1)

        isWallTile (x, y)
          | x < 0 = OutOfBounds
          | y < 0 = OutOfBounds
          | x >= w = OutOfBounds
          | y >= h = OutOfBounds
          | tileAtW lvl (x, y) == Wall = WallTile
          | otherwise = FloorTile --TODO add check if is unreachable tile

------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

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
  R == Root
  W == Wall
  T == Tile
  O == OutOfBounds
  A == T <|> O
-}
-----------------------
-- CenterWall
-----------------------
isSurroundedWallTextureTile :: WallNeighbors -> Bool
isSurroundedWallTextureTile = (==) surroundedWall

surroundedWall =
  Layout
    [ [WallTile, WallTile, WallTile],
      [WallTile, Root, WallTile],
      [WallTile, WallTile, WallTile]
    ]

-----------------------
-- Straight
-----------------------
{-
  Straight  Cases:  (Bothsides A's) Rotations (4)
  A A A
  W W W
  W W W

  Beside Corner Cases: Rotations (4) Mirrored (2)
  A A W
  W W W
  W W W
-}

isStraightTextureTile :: WallNeighbors -> Bool
isStraightTextureTile = (`elem` fullSideFloors ++ cornerAndSideFloors)

fullSideFloors =
  let fullSideFloor =
        Layout
          [ [FloorTile, FloorTile, FloorTile],
            [WallTile, Root, WallTile],
            [WallTile, WallTile, WallTile]
          ]
   in rotRNTimes 4 fullSideFloor

cornerAndSideFloors =
  let cornerAndSideFloor =
        Layout
          [ [FloorTile, FloorTile, WallTile],
            [WallTile, Root, WallTile],
            [WallTile, WallTile, WallTile]
          ]
   in (rotRNTimes 4 cornerAndSideFloor) ++ (rotRNTimes 4 (mirrorH cornerAndSideFloor))

-----------------------
-- Straight Single
-----------------------
{-
  StraightSingle  Cases:  (Bothsides A's) Rotations (2)
  A A A
  W W W
  A A A

  StraighSingle case Oposite Beside Corner Cases: Rotations (2) Mirrored (2)
  A A W
  W W W
  W A A

  StraignSingle Case Same Side CornerAndSide Cases: Rotations (4)
  W W W
  A W A
  A W W

 StraignSingle Case L-Shape Cases: Rotations (4) Mirrored (2)
  A W A
  A W A
  A W W

  StraignSingle Case Helmet Shape Middle Cases: Rotations (4)
  A A A
  W W W
  W A W

  StraignSingle Case h-Shape Cases: Rotations (4) Mirrored (2)
  A A W
  W W W
  W A W

  StraignSingle Case H-Shape Cases: Rotations (4)
  W A W
  W W W
  W A W

-}
isStraightSingleTextureTile :: WallNeighbors -> Bool
isStraightSingleTextureTile =
  ( `elem`
      fullBothSidesFloors
        ++ opositeCornerAndSideFloors
        ++ sameSideCornerAndSideFloors
        ++ straightLShapeFloors
        ++ straightHelmetShapeFloors
        ++ straighthShapeFloors
        ++ straightHShapeFloors
  )

fullBothSidesFloors =
  let fullBothSidesFloor =
        Layout
          [ [FloorTile, FloorTile, FloorTile],
            [WallTile, Root, WallTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 2 fullBothSidesFloor

opositeCornerAndSideFloors =
  let opositeCornerAndSideFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [FloorTile, Root, FloorTile],
            [WallTile, WallTile, FloorTile]
          ]
   in (rotRNTimes 2 opositeCornerAndSideFloor) ++ rotRNTimes 2 (mirrorH opositeCornerAndSideFloor)

sameSideCornerAndSideFloors =
  let sameSideCornerAndSideFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [FloorTile, Root, FloorTile],
            [WallTile, WallTile, FloorTile]
          ]
   in rotRNTimes 4 sameSideCornerAndSideFloor

straightLShapeFloors =
  let straightLShapeFloor =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [FloorTile, Root, FloorTile],
            [FloorTile, WallTile, WallTile]
          ]
   in rotRNTimes 4 straightLShapeFloor ++ rotRNTimes 4 (mirrorH straightLShapeFloor)

straightHelmetShapeFloors =
  let straightHelmetShapeFloor =
        Layout
          [ [FloorTile, FloorTile, FloorTile],
            [WallTile, Root, WallTile],
            [WallTile, FloorTile, WallTile]
          ]
   in rotRNTimes 4 straightHelmetShapeFloor

straighthShapeFloors =
  let straighthShapeFloor =
        Layout
          [ [FloorTile, FloorTile, WallTile],
            [WallTile, Root, WallTile],
            [WallTile, FloorTile, WallTile]
          ]
   in rotRNTimes 4 straighthShapeFloor ++ rotRNTimes 4 (mirrorH straighthShapeFloor)

straightHShapeFloors =
  let straightHShapeFloor =
        Layout
          [ [WallTile, FloorTile, WallTile],
            [WallTile, Root, WallTile],
            [WallTile, FloorTile, WallTile]
          ]
   in rotRNTimes 4 straightHShapeFloor

-----------------------
-- Corner
-----------------------
{-
  Corner Cases:
  A W W
  W W W
  W W W

  BigFloorCorner Cases: Rotations (4)
  A W W
  A W W
  A A A

  Small FloorCorner Cases: Rotations (4)
  W W W
  A W W
  A A W

  L-FloorCorner Cases: Rotations (4) Mirrored (2)
  A W W
  A W W
  A A W

-- TODO make own type
  singleToDoubleCorner Cases: Rotations (4) Mirrored (2)
  A W W
  W W W
  A A A
-}

isCornerTextureTile :: WallNeighbors -> Bool
isCornerTextureTile = (`elem` cornerFloors ++ bigCornerFloors ++ smallCornerFloors ++ lShapedCornerFloors ++ singleToDoubleCornerFloors)

cornerFloors =
  let cornerFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [WallTile, Root, WallTile],
            [WallTile, WallTile, WallTile]
          ]
   in rotRNTimes 4 cornerFloor

bigCornerFloors =
  let bigCornerFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [FloorTile, Root, WallTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 bigCornerFloor

smallCornerFloors =
  let smallCornerFloor =
        Layout
          [ [WallTile, WallTile, WallTile],
            [FloorTile, Root, WallTile],
            [FloorTile, FloorTile, WallTile]
          ]
   in rotRNTimes 4 smallCornerFloor

lShapedCornerFloors =
  let lShapedCornerFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [FloorTile, Root, WallTile],
            [FloorTile, FloorTile, WallTile]
          ]
   in (rotRNTimes 4 lShapedCornerFloor) ++ (rotRNTimes 4 $ mirrorH lShapedCornerFloor)

-- special case - TODO make own type
singleToDoubleCornerFloors =
  let singleToDoubleCornerFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [WallTile, Root, WallTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in (rotRNTimes 4 singleToDoubleCornerFloor) ++ (rotRNTimes 4 $ mirrorH singleToDoubleCornerFloor)

-----------------------
-- CornerSingle
-----------------------
{-
  CornerSingle Cases: Rotations (4)
  A W A
  W W A
  A A A

  CornerTwoSeparateFloors Cases: Rotations (4) (Special case for single corner that closes diaonally)
  A W A
  W W A
  A A W

  CornerAndOpositeSmallCorner Cases: Rotations (4) (Special case of single to maybe multiple corners)
  A W W
  W W A
  W A A

  -- Corner But has underconnection to single Cases: Rotations (4)
  --   A W W
  --   A W W
  --   W W A
-}

isCornerSingleTextureTile :: WallNeighbors -> Bool
isCornerSingleTextureTile = (`elem` cornerSingleFloors ++ cornerSingleDiagonalFloors ++ cornerSingleZigzagFloors)

cornerSingleFloors =
  let cornerSingleFloor =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [WallTile, Root, FloorTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 cornerSingleFloor

-- Special Case :: CornerSingle With diagonal Connection
cornerSingleDiagonalFloors =
  let cornerSingleDiagonalFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [WallTile, Root, FloorTile],
            [WallTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 cornerSingleDiagonalFloor

-- Special Case :: CornerSingle With diagonal zigzaging Connection
cornerSingleZigzagFloors =
  let cornerSingleZigzagFloor =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [WallTile, Root, FloorTile],
            [WallTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 cornerSingleZigzagFloor

-----------------------
-- T-Junction Single
-----------------------
{-
  T-JunctionSingle Cases: Rotations (4)
  A W A
  W W W
  A A A
-}

isTJunctionSingleTextureTile :: WallNeighbors -> Bool
isTJunctionSingleTextureTile = (`elem` tJunctionSingles)

tJunctionSingles =
  let tJunctionSingle =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [WallTile, Root, WallTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 tJunctionSingle

-----------------------
-- T-Junction
-----------------------

{-
  T-Junction Cases: Rotations (4)
  A W A
  W W W
  W W W
-}

isTJunctionTextureTile :: WallNeighbors -> Bool
isTJunctionTextureTile = (`elem` tJunctions)

tJunctions =
  let tJunction =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [WallTile, Root, WallTile],
            [WallTile, WallTile, WallTile]
          ]
   in rotRNTimes 4 tJunction

-----------------------
-- CrossSection
-----------------------
{-
  CrossSectionSingle:
  A W A
  W W W
  A W A
-}

isCrossSectionTextureTile :: WallNeighbors -> Bool
isCrossSectionTextureTile = (`elem` crossSections)

crossSections =
  let crossSection =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [WallTile, Root, WallTile],
            [FloorTile, WallTile, FloorTile]
          ]
   in rotRNTimes 4 crossSection

-----------------------
-- EndingSingle
-----------------------
{-
  EndingSingle Cases: Rotations (4) (looks like U-junction of floors)
  A W A
  A W A
  A A A

  HelmetEndingSingle Cases: Rotations (4) (looks like small U-junction of floors)
  W W W
  A W A
  A A A

  EndingSingle Cases: Rotations (4) Mirror (2) (looks like J-junction of floors)
  A W W
  A W A
  A A A
-}

isEndingSingleTextureTile :: WallNeighbors -> Bool
isEndingSingleTextureTile = (`elem` fullEndingSingles ++ shortEndingSingles ++ partialEndingSingles)

fullEndingSingles =
  let fullEndingSingle =
        Layout
          [ [FloorTile, WallTile, FloorTile],
            [FloorTile, Root, FloorTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 fullEndingSingle

shortEndingSingles =
  let shortEndingSingle =
        Layout
          [ [WallTile, WallTile, WallTile],
            [FloorTile, Root, FloorTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 shortEndingSingle

partialEndingSingles =
  let partialEndingSingle =
        Layout
          [ [FloorTile, WallTile, WallTile],
            [FloorTile, Root, FloorTile],
            [FloorTile, FloorTile, FloorTile]
          ]
   in rotRNTimes 4 partialEndingSingle

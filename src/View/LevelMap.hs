module View.LevelMap where

import Data.List (elemIndex)
import Data.List.Index (imap)
import Model.Level

-- | Rotation direction of a tile
-- | Default tile rotation is North
data Rotation = N | E | S | W deriving (Eq, Show, Enum)

-- | Indicates if a tile's texture should be rendered flipped
-- | Default tile mirroring is NotMirrored
data Mirrored = Mirrored | NotMirrored deriving (Eq, Show, Enum)

-- | Texture types of tiles in a level
data TextureTile
  = None
  | Straight Rotation
  | StraightSingle -- Rotation
  | Corner -- Rotation
  | CornerSingle -- Rotation
  | CornerSingleToDouble -- Rotation Mirrored
  | CrossSectionSingle
  | CrossSectionFishShaped -- Rotation
  | Tjunction -- Rotation
  | TjunctionSingle -- Rotation
  | SurroundedWall
  | EndingSingle -- Rotation
  | GhostDoorStraight -- Rotation-- TODO add pattern
  | GhostDoorCorner -- Rotation-- TODO add pattern
  | Dev --Fallback type
  deriving (Show, Eq)

-- | Type alias for a Layout with the reachability of a tile and its surrounding tiles
type WallNeighbors = Layout Rachability

-- | Data type defining if a tile is reachable by a player
data Rachability = Reachable | Unreachable deriving (Show, Eq)

-- | Converts a level to a layout of texture tiles
-- | The layout can be interpreted for specific texture rendering
convertLevel :: Level -> Layout TextureTile
convertLevel lvl@Level {layout = Layout xss} = Layout (imap (\y -> imap (\x -> convert (x, y))) xss)
  where
    convert :: (Int, Int) -> Tile -> TextureTile
    convert pos tile = case tile of
      Wall -> generateTextureTile pos
      GhostDoor _ -> GhostDoorStraight
      _ -> None

    generateTextureTile :: (Int, Int) -> TextureTile
    generateTextureTile (x, y)
      | isResult isStraightRes = Straight $ toEnum n
      -- | isStraightTextureTile matrix = Straight
      | isStraightSingleTextureTile matrix = StraightSingle
      | isCornerTextureTile matrix = Corner
      | isCornerSingleTextureTile matrix = CornerSingle
      | isTJunctionSingleTextureTile matrix = TjunctionSingle
      | isTJunctionTextureTile matrix = Tjunction
      | isCrossSectionTextureTile matrix = CrossSectionSingle
      | isSurroundedWallTextureTile matrix = SurroundedWall
      | isEndingSingleTextureTile matrix = EndingSingle
      | isCornerSingleToDoubleFloorsTextureTile matrix = CornerSingleToDouble
      | isCrossSectionFishShapedFloorsTextureTile matrix = CrossSectionFishShaped
      | otherwise = Dev
      where
        matrix = generateWallNeighbors (x, y)

        -- TODO find a way to check on NonOp if is Res then take value n from res, else ignore
        -- MAAybe een data type die handled if res else do next NonOp Calculation (andere pattern set met andere result handling?)
        -- handleIfRes isStraightRes (\n Straight toEnum n) (\nextpattern....)
        isStraightRes = isStraightTextureTile matrix

        isResult :: NonOp a b -> Bool
        isResult Res a = True
        isResult _ = False


    generateWallNeighbors :: (Int, Int) -> WallNeighbors
    generateWallNeighbors (x, y) =
      Layout
        [ [ul, u, ur],
          [l, Unreachable, r],
          [dl, d, dr]
        ]
      where
        (w, h) = layoutSize . layout $ lvl

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

--TODO : (OPTIONAL) : add check if is unreachable tile, if so then unreachable
--        (so that unrechable floors can act as walls)

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
-}
-----------------------
-- CenterWall
-----------------------
isSurroundedWallTextureTile :: WallNeighbors -> Bool
isSurroundedWallTextureTile = (==) surroundedWallFloors

surroundedWallFloors =
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
  ▊▊◯
  ▊▊◯
  ▊▊◯

  Beside Corner Cases: Rotations (4) Mirrored (2)
  ▊▊▊
  ▊▊◯
  ▊▊◯
-}

-- data Mayb a b 
--   = QueryVal a 
--   | Res b 
--   deriving (Eq)

data NonOp a b = Res a 
  | QueryVal b 
  | Func (b -> Maybe a)
  
  

apply :: NonOp a b -> NonOp a b -> NonOp a b
apply (Res b) _ = Res b --Maybe flip precendence?
apply (Func f) (Res b) = Res b
apply (Func f) (QueryVal a) = case f a of
  Just x -> Res x
  Nothing -> QueryVal a
apply (QueryVal a) (Func f) = case f a of
  Just x -> Res x
  Nothing -> QueryVal a
apply (Func f) (Func g) = Func (f) -- Maybe return function with precedence, due to applying func on func shouldt interesting result.
-- apply (Func f) (Func g) = Func (f . g)

($$) :: NonOp a b -> NonOp a b -> NonOp a b
($$) = apply

-- t = g $$ f $$ QueryValue 
-- instance Functor (NonOp a) where
--   fmap _ (Res x) = Res x
--   fmap f (QueryVal y) = QueryVal (f y)

-- instance Applicative (NonOp a) where
--   pure x = QueryVal x
--   Res y <*> _ = Res y 
--   QueryVal x <*> something = fmap x something

-- isInIndex :: WallNeighbors -> [WallNeighbors] -> (Bool, Int)
-- isInIndex wn wns = case wn `elemIndex` wns of 
--   Just n -> (True, n `mod` 4)
--   Nothing -> (False, -1)
--   where
--     mIndex = wn `elemIndex` (fullSideFloors ++ cornerAndSideFloors)

isStraightTextureTile :: WallNeighbors -> NonOp WallNeighbors Int --(Bool, Int)
isStraightTextureTile wn = (`elemIndex` fullSideFloors) $$ (`elemIndex` cornerAndSideFloors) $$ wn
-- isStraightTextureTile wn = wn `isInIndex` (fullSideFloors ++ cornerAndSideFloors)

fullSideFloors =
  let fullSideFloor =
        Layout
          [ [Unreachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable]
          ]
   in rotRNTimes 4 fullSideFloor

cornerAndSideFloors =
  let cornerAndSideFloor =
        Layout
          [ [Unreachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable]
          ]
   in rotRNTimes 4 cornerAndSideFloor ++ rotRNTimes 4 (mirrorH cornerAndSideFloor)

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
  ▊▊▊
  ◯▊◯
  ◯▊▊

 StraightSingle Case L-Shape Cases: Rotations (4) Mirrored (2)
  ◯▊◯
  ◯▊◯
  ◯▊▊

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
          [ [Reachable, Reachable, Reachable],
            [Unreachable, Unreachable, Unreachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 2 fullBothSidesFloor

opositeCornerAndSideFloors =
  let opositeCornerAndSideFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable]
          ]
   in rotRNTimes 2 opositeCornerAndSideFloor ++ rotRNTimes 2 (mirrorH opositeCornerAndSideFloor)

sameSideCornerAndSideFloors =
  let sameSideCornerAndSideFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable]
          ]
   in rotRNTimes 4 sameSideCornerAndSideFloor

straightLShapeFloors =
  let straightLShapeFloor =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Reachable, Unreachable, Reachable],
            [Reachable, Unreachable, Unreachable]
          ]
   in rotRNTimes 4 straightLShapeFloor ++ rotRNTimes 4 (mirrorH straightLShapeFloor)

straightHelmetShapeFloors =
  let straightHelmetShapeFloor =
        Layout
          [ [Reachable, Reachable, Reachable],
            [Unreachable, Unreachable, Unreachable],
            [Unreachable, Reachable, Unreachable]
          ]
   in rotRNTimes 4 straightHelmetShapeFloor

straighthShapeFloors =
  let straighthShapeFloor =
        Layout
          [ [Reachable, Reachable, Unreachable],
            [Unreachable, Unreachable, Unreachable],
            [Unreachable, Reachable, Unreachable]
          ]
   in rotRNTimes 4 straighthShapeFloor ++ rotRNTimes 4 (mirrorH straighthShapeFloor)

straightHShapeFloors =
  let straightHShapeFloor =
        Layout
          [ [Unreachable, Reachable, Unreachable],
            [Unreachable, Unreachable, Unreachable],
            [Unreachable, Reachable, Unreachable]
          ]
   in rotRNTimes 4 straightHShapeFloor

-----------------------
-- Corner
-----------------------
{-
  Corner Cases:
  ◯▊▊
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

isCornerTextureTile :: WallNeighbors -> Bool
isCornerTextureTile = (`elem` cornerFloors ++ bigCornerFloors ++ smallCornerFloors ++ lShapedCornerFloors)

cornerFloors =
  let cornerFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Unreachable]
          ]
   in rotRNTimes 4 cornerFloor

bigCornerFloors =
  let bigCornerFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Unreachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 bigCornerFloor

smallCornerFloors =
  let smallCornerFloor =
        Layout
          [ [Unreachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Unreachable],
            [Reachable, Reachable, Unreachable]
          ]
   in rotRNTimes 4 smallCornerFloor

lShapedCornerFloors =
  let lShapedCornerFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Unreachable],
            [Reachable, Reachable, Unreachable]
          ]
   in rotRNTimes 4 lShapedCornerFloor ++ rotRNTimes 4 (mirrorH lShapedCornerFloor)

-----------------------
-- CornerSingle
-----------------------
{-
  CornerSingle Cases: Rotations (4)
  ◯▊◯
  ▊▊◯
  ◯◯◯

  CornerTwoSeparateFloors Cases: Rotations (4) (Special case for single corner that closes diaonally)
  ◯▊◯
  ▊▊◯
  ◯◯▊

  CornerAndOpositeSmallCorner Cases: Rotations (4) (Special case of single to maybe multiple corners)
  ◯▊▊
  ▊▊◯
  ▊◯◯

  -- Corner But has underconnection to single Cases: Rotations (4)
  --   ◯▊▊
  --   ◯▊▊
  --   ▊▊◯
-}

isCornerSingleTextureTile :: WallNeighbors -> Bool
isCornerSingleTextureTile = (`elem` cornerSingleFloors ++ cornerSingleDiagonalFloors ++ cornerSingleZigzagFloors)

cornerSingleFloors =
  let cornerSingleFloor =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Reachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 cornerSingleFloor

-- Special Case :: CornerSingle With diagonal Connection
cornerSingleDiagonalFloors =
  let cornerSingleDiagonalFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Reachable],
            [Unreachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 cornerSingleDiagonalFloor

-- Special Case :: CornerSingle With diagonal zigzaging Connection
cornerSingleZigzagFloors =
  let cornerSingleZigzagFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Reachable],
            [Unreachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 cornerSingleZigzagFloor

-----------------------
-- CornerSingleToDouble
-----------------------
{-
  CornerSingleToDouble Cases: Rotations (4) Mirrored (2)
  ◯▊▊
  ▊▊▊
  ◯◯◯
-}

isCornerSingleToDoubleFloorsTextureTile :: WallNeighbors -> Bool
isCornerSingleToDoubleFloorsTextureTile = (`elem` singleToDoubleCornerFloors)

singleToDoubleCornerFloors =
  let singleToDoubleCornerFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Unreachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 singleToDoubleCornerFloor ++ rotRNTimes 4 (mirrorH singleToDoubleCornerFloor)

-----------------------
-- T-Junction Single
-----------------------
{-
  T-JunctionSingle Cases: Rotations (4)
  ◯▊◯
  ▊▊▊
  ◯◯◯
-}

isTJunctionSingleTextureTile :: WallNeighbors -> Bool
isTJunctionSingleTextureTile = (`elem` tJunctionSingles)

tJunctionSingles =
  let tJunctionSingle =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Unreachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 tJunctionSingle

-----------------------
-- T-Junction
-----------------------

{-
  T-Junction Cases: Rotations (4)
  ◯▊◯
  ▊▊▊
  ▊▊▊
-}

isTJunctionTextureTile :: WallNeighbors -> Bool
isTJunctionTextureTile = (`elem` tJunctions)

tJunctions =
  let tJunction =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Unreachable]
          ]
   in rotRNTimes 4 tJunction

-----------------------
-- CrossSection
-----------------------
{-
  CrossSectionSingle:
  ◯▊◯
  ▊▊▊
  ◯▊◯
-}

isCrossSectionTextureTile :: WallNeighbors -> Bool
isCrossSectionTextureTile = (`elem` crossSections)

crossSections =
  let crossSection =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Unreachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable]
          ]
   in rotRNTimes 4 crossSection

-----------------------
-- CrossSectionFishShaped
-----------------------
{-
  CrossSectionFishShaped Cases: Rotations (4)
  ◯▊▊
  ▊▊▊
  ◯▊◯
-}

isCrossSectionFishShapedFloorsTextureTile :: WallNeighbors -> Bool
isCrossSectionFishShapedFloorsTextureTile = (`elem` crossSectionFishShapedFloors)

crossSectionFishShapedFloors =
  let crossSectionFishShapedFloor =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Unreachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable]
          ]
   in rotRNTimes 4 crossSectionFishShapedFloor

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

isEndingSingleTextureTile :: WallNeighbors -> Bool
isEndingSingleTextureTile = (`elem` fullEndingSingles ++ shortEndingSingles ++ partialEndingSingles)

fullEndingSingles =
  let fullEndingSingle =
        Layout
          [ [Reachable, Unreachable, Reachable],
            [Reachable, Unreachable, Reachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 fullEndingSingle

shortEndingSingles =
  let shortEndingSingle =
        Layout
          [ [Unreachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 shortEndingSingle

partialEndingSingles =
  let partialEndingSingle =
        Layout
          [ [Reachable, Unreachable, Unreachable],
            [Reachable, Unreachable, Reachable],
            [Reachable, Reachable, Reachable]
          ]
   in rotRNTimes 4 partialEndingSingle

-- TODO
-----------------------
-- GhostDoorStraight
-----------------------
{-
  GhostDoorStraight Cases:
  ◯▊
-}

-----------------------
-- GhostDoorCorner
-----------------------
{-
  GhostDoorStraight Cases:
  ◯▊
-}

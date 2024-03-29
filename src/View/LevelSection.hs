{-# LANGUAGE MultiWayIf #-}

module View.LevelSection (renderLevelSection) where

import Data.Maybe
import Graphics.Gloss
import Model.Game
import Model.Ghosts as G
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player hiding (position)
import Model.Utils (imap)
import View.Animation
import View.Config
import View.Debug
import View.Helpers
import View.LevelMap

-- | Returns Pictures, consisting of all tile Pictures
renderLevelSection :: Bool -> Textures -> GameState -> Picture
renderLevelSection isDebug textures gs = translateToLevelSection (layoutSize . layout . level $ gs) . pictures $ map ($ gs) fs
  where
    fs =
      [ renderLevel isDebug textures . level,
        renderIntersections isDebug,
        renderPellets . items . level,
        renderFruit textures . items . level,
        renderGhosts textures,
        renderPlayer textures
      ]

renderMovable :: Movable a => a -> Direction -> Layout Tile -> Picture -> Picture
renderMovable m dir ll = translateByTileSize x y
  where
    (px, py) = getPosition m
    (x, y) = case dir of
      M.Up -> roundHorizontal
      M.Down -> roundHorizontal
      M.Left -> roundVertical
      M.Right -> roundVertical
      _ -> (fromIntegral $ round px, fromIntegral $ round py)
    roundHorizontal = (fromIntegral $ round px, py)
    roundVertical = (px, fromIntegral $ round py)

renderPlayer :: Textures -> GameState -> Picture
renderPlayer textures gs = renderMovable pl dir ll . pacMan textures $ dir
  where
    pl = player gs
    dir = Model.Player.direction . player $ gs
    ll = layout $ level gs

-- | Returns Pictures (Picture consisting of multiple pictures
renderGhosts :: Textures -> GameState -> Picture
renderGhosts t gs = pictures . map renderGhost $ ghosts gs
  where
    renderGhost g = renderMovable g (G.direction g) ll $ ghost t (ghostMode gs) (frightenedTime gs) g
    ll = layout $ level gs

-- | Returns Pictures (Picture consisting of multiple pictures)
renderPellets :: [PointItem] -> Picture
renderPellets = pictures . map renderItem
  where
    dotColor = light . light . light . light $ yellow
    renderItem item = let (x, y) = getPosition item in translateByTileSize x y (toPicture item)
    toPicture item = case item of
      Dot _ _ -> color dotColor . circleSolid $ tileSize / 8
      PowerPellet _ _ -> color dotColor . circleSolid $ tileSize / 3
      _ -> Blank

renderFruit :: Textures -> [PointItem] -> Picture
renderFruit textures xs = pictures $ [renderItem x | x@Fruit {} <- xs]
  where
    renderItem item =
      let (x, y) = getPosition item
       in translateByTileSize x y (fruitTexture textures item)

renderLevel :: Bool -> Textures -> Level -> Picture
renderLevel isDebug textures lvl =
  pictures
    . catMaybes
    . concat
    $ imap (\y -> imap (imapFunc y)) xss
  where
    renderFunction =
      if isDebug
        then devRenderTile
        else renderTile
    (Layout xss) = fmap (renderFunction $ tileTextures textures) $ convertLevel lvl
    imapFunc :: Int -> Int -> Maybe Picture -> Maybe Picture
    imapFunc y x mt = case mt of
      Just t -> Just $ translateByTileSize (fromIntegral x) (fromIntegral y) t
      Nothing -> Nothing

-- -- | Renders tile textures
-- renderTile :: TileTextures -> TextureTile -> Maybe Picture
-- renderTile tTextures tTile =

-- Dev tile render to visualise different tile types in level
devRenderTile :: TileTextures -> TextureTile -> Maybe Picture
devRenderTile _ tTile = case tTile of
  None -> Nothing
  Straight _ -> Just $ color orange $ rectangleSolid tileSize tileSize
  StraightSingle _ -> Just $ color blue $ rectangleSolid tileSize tileSize
  Corner _ -> Just $ color red $ rectangleSolid tileSize tileSize
  CornerSingle _ -> Just $ color cyan $ rectangleSolid tileSize tileSize
  CornerSingleToDouble _ _ -> Just $ color chartreuse $ rectangleSolid tileSize tileSize
  CrossSectionSingle -> Just $ color aquamarine $ rectangleSolid tileSize tileSize
  CrossSectionFishShaped _ -> Just $ color rose $ rectangleSolid tileSize tileSize
  SurroundedWall -> Just $ color yellow $ rectangleSolid tileSize tileSize
  Tjunction _ -> Just $ color green $ rectangleSolid tileSize tileSize
  TjunctionSingle _ -> Just $ color violet $ rectangleSolid tileSize tileSize
  EndingSingle _ -> Just $ color azure $ rectangleSolid tileSize tileSize
  GhostDoorStraight _ -> Just $ color green $ rectangleSolid tileSize tileSize
  GhostDoorCorner _ -> Just $ color (bright green) $ rectangleSolid tileSize tileSize
  Dev -> Just $ color magenta $ rectangleSolid tileSize tileSize

rotPicture :: Rotation -> Picture -> Picture
rotPicture r = rotate ((fromIntegral $ fromEnum r) * 90)

-- | Actual tile render
renderTile :: TileTextures -> TextureTile -> Maybe Picture
renderTile tTextures tTile = case tTile of
  None -> Nothing
  -- SurroundedWall -> Just $ surroundedWall tTextures
  SurroundedWall -> Just blank
  Straight rot -> Just (rotPicture rot $ straight tTextures)
  StraightSingle rot -> Just (rotPicture rot $ straightSingle tTextures)
  Corner rot -> Just (rotPicture rot $ corner tTextures)
  CornerSingle rot -> Just (rotPicture rot $ cornerSingle tTextures)
  CornerSingleToDouble rot m -> Just $
    rotPicture rot $ case m of
      NotMirrored -> cornerSingleToDouble tTextures
      Mirrored -> cornerSingleToDoubleMirrored tTextures
  CrossSectionSingle -> Just (crossSectionSingle tTextures)
  CrossSectionFishShaped rot -> Just (rotPicture rot $ crossSectionFishShaped tTextures)
  Tjunction rot -> Just (rotPicture rot $ tJunction tTextures)
  TjunctionSingle rot -> Just (rotPicture rot $ tJunctionSingle tTextures)
  EndingSingle rot -> Just (rotPicture rot $ endSingle tTextures)
  GhostDoorStraight rot -> Just (rotPicture rot $ ghostDoorStraight tTextures)
  GhostDoorCorner rot -> Just (rotPicture rot $ ghostDoorCorner tTextures)
  Dev -> Just $ missingTexture tTextures

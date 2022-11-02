{-# LANGUAGE MultiWayIf #-}

module View.LevelSection (renderLevelSection) where

import Data.List.Index (imap)
import Data.Maybe
import Graphics.Gloss
import Model.Game
import Model.Ghosts as G
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player hiding (position)
import View.Animation
import View.Config
import View.Debug
import View.Helpers
import View.LevelMap

-- | Returns Pictures, consisting of all tile Pictures
renderLevelSection :: Textures -> GameState -> Picture
renderLevelSection textures gs = translateToLevelSection (layoutSize . layout . level $ gs) . pictures $ map ($ gs) fs
  where
    fs =
      [ renderLevel textures . level,
        renderIntersections,
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

renderLevel :: Textures -> Level -> Picture
renderLevel textures lvl =
  pictures
    . catMaybes
    . concat
    $ imap (\y -> imap (imapFunc y)) xss
  where
    (Layout xss) = fmap (renderTile textures) $ convertLevel lvl

    imapFunc :: Int -> Int -> Maybe Picture -> Maybe Picture
    imapFunc y x mt = case mt of
      Just t -> Just $ translateByTileSize (fromIntegral x) (fromIntegral y) t
      Nothing -> Nothing

renderTile :: Textures -> TextureTile -> Maybe Picture
renderTile textures tile = case tile of
  None -> Nothing
  Straight -> Just $ color orange $ rectangleSolid tileSize tileSize
  StraightSingle -> Just $ color blue $ rectangleSolid tileSize tileSize
  Corner -> Just $ color red $ rectangleSolid tileSize tileSize
  CornerSingle -> Just $ color cyan $ rectangleSolid tileSize tileSize
  Tjunction -> Just $ color green $ rectangleSolid tileSize tileSize
  TjunctionSingle -> Just $ color violet $ rectangleSolid tileSize tileSize
  CrossSectionSingle -> Just $ color aquamarine $ rectangleSolid tileSize tileSize  
  SurroundedWall -> Just $ color yellow $ rectangleSolid tileSize tileSize
  EndingSingle -> Just $ color azure $ rectangleSolid tileSize tileSize
  SingleToDoubleCorner -> Just $ color chartreuse  $ rectangleSolid tileSize tileSize
  FishShapeCorner -> Just $ color rose  $ rectangleSolid tileSize tileSize
  Dev -> Just $ color magenta $ rectangleSolid tileSize tileSize

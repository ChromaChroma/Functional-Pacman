module View.LevelSection (renderLevelSection) where

import Data.List.Index
import Data.Maybe
import Graphics.Gloss
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player hiding (position)
import View.Config
import View.Debug
import View.Helpers
import View.Animation ( Textures, pacMan, ghost, fruitTexture )
import Model.Game
import Model.Ghosts as G

renderLevelSection :: Textures -> GameState -> Picture
renderLevelSection textures gs = translateToLevelSection (layoutSize . layout . level $ gs) . pictures $ map ($ gs) fs
  where
    fs =
      [ renderLevel . level,
        renderIntersections,
        renderPellets . items . level,
        renderFruit textures . items . level,
        renderGhosts textures,
        renderPlayer textures
      ]

-- | Returns Pictures, consisting of all tile Pictures
renderLevel :: Level -> Picture
renderLevel level = matrixToTilePitures $ layout level
  where
    matrixToTilePitures = pictures . catMaybes . concat . imap rowToTilePictures
    rowToTilePictures y = imap (`renderTile` y)

renderTile :: Int -> Int -> Tile -> Maybe Picture
renderTile x y tile = case tile of
  Wall -> Just $ color blue block
  GhostDoor Open -> Just $ color green block
  GhostDoor Closed -> Just $ color green block
  _ -> Nothing
  where
    block = translateByTileSize (fromIntegral x) (fromIntegral y) (rectangleSolid tileSize tileSize) -- Vertical lined walls
    vLinedBlock = translateByTileSize (fromIntegral x) (fromIntegral y) (rectangleSolid (tileSize / 2) tileSize) -- Vertical lined walls
    hLinedBlock = translateByTileSize (fromIntegral x) (fromIntegral y) (rectangleSolid tileSize (tileSize / 2)) -- Horizontal lined walls
    blockyWalls = translateByTileSize (fromIntegral x) (fromIntegral y) (rectangleSolid (tileSize / 2) (tileSize / 2)) -- small block walls
    -- todo Possibility: Render layout by converting layout to ajacent dots that create figures that can be rendered as a line/polygon pictures.

renderMovable :: Movable a => a -> Direction -> Layout -> Picture -> Picture
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


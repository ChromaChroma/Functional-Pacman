module View.LevelSection (renderLevelSection) where

import Data.Maybe
import Data.List.Index
import Graphics.Gloss

import View.Helpers
import View.Config
import Model.Game
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player hiding (position)

renderLevelSection :: GameState -> Picture
renderLevelSection gs = 
  let fs = [ 
        renderLevel . level,
        renderGhosts,
        renderPlayer,
        renderItems . items . level
        ] 
  in translateToLevelSection . pictures $ map ($ gs) fs

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

renderPlayer :: GameState -> Picture
renderPlayer gs = renderMovable pl dir ll . color yellow . circleSolid $ tileSize / 2
  where
    pl = player gs
    dir = direction gs
    ll = layout $ level gs

-- | Returns Pictures (Picture consisting of multiple pictures
renderGhosts :: GameState -> Picture
renderGhosts gs = pictures . map renderGhost . ghosts $ gs
  where
    renderGhost g = renderMovable g Stop ll . color red . circleSolid $ tileSize / 2
    ll = layout $ level gs

-- | Returns Pictures (Picture consisting of multiple pictures)
renderItems :: [PointItem] -> Picture
renderItems = pictures . map trans
  where
    trans item = let (x, y) = getPosition item in translate x y (renderItem item)
    renderItem pic = case pic of
      Dot _ _ -> color white . circleSolid $ tileSize / 8
      PowerPellet _ _ -> color white . circleSolid $ tileSize / 4
      _ -> Blank --Fruit ignored for now

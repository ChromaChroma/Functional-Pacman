module View.Gui where

import Controller.Engine
import Controller.MovementController (canMovePerpendicular, formatDecimals)
import Data.Fixed
import Data.List
import Data.List.Index
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game (Key (SpecialKey), SpecialKey (KeyEsc))
import Graphics.Gloss.Interface.IO.Game as IO
import Model.Game
import Model.Items
import Model.Level
import Model.Movement as M
import Model.Player
import Model.Score
import Numeric
import View.Config

startRender :: IO ()
startRender =
  play
    screen
    black
    fps
    initialModel
    drawingFunc
    inputHandler
    tickEngine

-- | Initial state of the game at startup
initialModel :: GameState
initialModel = defaultGame

-- | Render game state, aligned from bottom left corner
drawingFunc :: GameState -> Picture
drawingFunc gs =
  fromBottomLeft $
    pictures
      [ translateToLevelSection $ renderGame gs,
        renderOverlay gs
      ]
  where
    (lx, ly) = layoutSize $ layout $ level gs
    (x, y) = windowSize
    x' = - fromIntegral (x `div` 2) + tileSize / 2
    y' = - fromIntegral (y `div` 2) + tileSize / 2
    fromBottomLeft = translate x' y'

    renderGame gs =
      pictures
        [ renderLevel . level $ gs,
          renderIntersections gs,
          renderGhosts gs,
          renderPlayer gs
          -- Render Movable?? ghost and player get rendered same way.
          -- renderItems . items . level $ gs
          -- PointItems
        ]
    renderOverlay gs =
      pictures
        [ -- renderLives . lives . player $ gs
          -- renderScore . score $ gs
          renderDebug gs
        ]

-- | Returns Pictures, consisting of all tile Pictures
renderLevel :: Level -> Picture
renderLevel level = matrixToTilePitures $ layout level
  where
    matrixToTilePitures = pictures . catMaybes . concat . imap rowToTilePictures . reverse
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
    (_, ly) = layoutSize ll
    (px, py) = getPosition m
    y' = fromIntegral ly - 1 - py

    (x, y) = case dir of
      M.Up -> roundHorizontal
      M.Down -> roundHorizontal
      M.Left -> roundVertical
      M.Right -> roundVertical
      _ -> (fromIntegral $ round px, fromIntegral $ round y')
    roundHorizontal = (fromIntegral $ round px, y')
    roundVertical = (px, fromIntegral $ round y')

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
renderItems = undefined

renderLives :: Lives -> [Picture]
renderLives = undefined

renderScore :: Score -> Picture
renderScore = undefined

renderDebug :: GameState -> Picture
renderDebug gs =
  let (x, y) = getPosition . player $ gs
   in pictures . stack 0 0 25 $
        reverse
          [ -- translate 0 200 . smallText . show . score $ gs,
            smallText "Status: " . status $ gs,
            smallText "Elapsed time (s): " . msToSec . elapsedTime $ gs,
            smallText "Tick time (ms): " . tickTimer $ gs,
            smallText "Lives: " . unlives . lives . player $ gs,
            smallText "Direction: " . direction $ gs,
            smallText "Buffer Direction: " . bufDirection $ gs,
            smallText "Position: " . getPosition . player $ gs,
            smallText "Coordinate decimals x, y: " $ show (formatDecimals x 1) ++ ", " ++ show (formatDecimals y 1),
            smallText "Can switch x, y: " $ (show . canMovePerpendicular $ x) ++ ", " ++ (show . canMovePerpendicular $ y),
            smallText "Intersections: " . levelIntersections . level $ gs
          ]

-- | Renders the intersections calculated by the game based on the level layout
renderIntersections :: GameState -> Picture
renderIntersections gs = pictures [block (x, y) | (x, y) <- levelIntersections . level $ gs]
  where
    block (x, y) = color (dim green) . translateByTileSize (fromIntegral x) y' $ rectangleSolid intersectionSize intersectionSize
      where
        (_, h) = layoutSize . layout . level $ gs
        y' = fromIntegral (h - 1 - y)
        intersectionSize = tileSize / 2

-- |
-- | Render Helper functions
-- |
translateToLevelSection :: Picture -> Picture
translateToLevelSection = translate 300 0

translateByTileSize :: Float -> Float -> Picture -> Picture
translateByTileSize x y = translate (x * tileSize) (y * tileSize)

smallText :: Show a => String -> a -> Picture
smallText name a = color white . scale 0.1 0.1 . text $ name ++ show a

stack :: Float -> Float -> Float -> [Picture] -> [Picture]
stack _ _ _ [] = []
stack x y pixels (l : ls) = translate x y l : stack x (y + pixels) pixels ls

msToSec :: Time -> Float
msToSec t = fromIntegral t / 1000

-- | Input handling
inputHandler :: Event -> GameState -> GameState

-- | Movement with arrow keys
inputHandler (EventKey (SpecialKey KeyUp) IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (SpecialKey KeyDown) IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (SpecialKey KeyRight) IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (SpecialKey KeyLeft) IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'w') IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (Char 's') IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (Char 'd') IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (Char 'a') IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'p') IO.Down _ _) gs = pause gs
inputHandler (EventKey (Char 'r') IO.Down _ _) gs = resume gs
inputHandler (EventKey (Char 'q') IO.Down _ _) gs = quit gs
inputHandler _ gs = gs

-- | Update function ran each iteration
-- | Takes seconds since last update as Float, converts it to milliseconds and passes it to the engine step function
tickEngine :: Float -> GameState -> GameState
tickEngine s gs = let ms = round (s * 1000) in tick ms gs
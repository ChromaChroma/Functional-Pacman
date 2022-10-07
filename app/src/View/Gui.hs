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

screen :: Display
screen = InWindow "Pac Man" windowSize windowOffsetPosition

-- screen = FullScreenpli

windowSize :: (Int, Int)
windowSize = (1300, 1000)

windowOffsetPosition :: (Int, Int)
windowOffsetPosition = (0, 0)

fps :: Int
fps = 60

tileSize :: Float
tileSize = 32

startRender :: IO ()
startRender =
  play
    screen
    black
    fps
    initialModel
    drawingFunc
    inputHandler
    updateFunc

-- | Initial state of the game at startup
initialModel :: GameState
initialModel = defaultGame

-- | Render game state, aligned from bottom left corner
drawingFunc :: GameState -> Picture
drawingFunc gs =
  fromBottomLeft $
    pictures
      [ translate 300 0 $ renderGame gs,
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
          renderGhosts gs,
          renderPlayer gs
          -- Render Movable?? ghost and player get rendered same way.
          -- renderGhosts . ghosts $ gs
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

translateByTileSize :: Float -> Float -> Picture -> Picture
translateByTileSize x y = translate (x * tileSize) (y * tileSize)

-- | Int based player render
-- renderPlayer :: GameState -> Picture
-- renderPlayer gs = translateByTileSize x' y' . color yellow . circleSolid $ tileSize/2
--     where
--         (_, ly) = layoutSize $ layout $ level gs
--         (x, y) = intPosition $ getPosition $ player gs
--         x' =  fromIntegral x
--         y' =  fromIntegral ly - fromIntegral (y-2)

-- | Renders on Float instead of Int (But does currently shwo pacman aligned up to 0.5 - 1.49 instead of 1.0, due to engine/ engine-to-view convertion)
-- renderPlayer :: GameState -> Picture
-- renderPlayer gs = translateByTileSize x y' . color yellow . circleSolid $ tileSize / 2
--     where
--         y' =  fromIntegral ly - (y-2)
--         (_, ly) = layoutSize $ layout $ level gs
--         (x, y) =  getPosition $ player gs

-- renderPlayer :: GameState -> Picture
-- renderPlayer gs = translateByTileSize x y . color yellow . circleSolid $ tileSize / 2
--     where

--         (_, ly) = layoutSize $ layout $ level gs
--         (px, py) =  getPosition $ player gs
--         y' =  fromIntegral ly - (py-2)

--         (x, y) = case direction gs of
--           M.Up    -> roundHorizontal
--           M.Down  -> roundHorizontal
--           M.Left  -> roundVertical
--           M.Right -> roundVertical
--           _       -> (fromIntegral $ round px, fromIntegral $ round  y')
--         roundHorizontal = (fromIntegral $ round px, y')
--         roundVertical   = (px, fromIntegral $ round y')

renderMovable :: Movable a => a -> Direction -> LevelLayout -> Picture -> Picture
renderMovable m dir ll = translateByTileSize x y
  where
    (_, ly) = layoutSize ll
    (px, py) = getPosition m
    y' = fromIntegral ly - (py -2)

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
  pictures . stack 0 0 25 $
    reverse
      [ -- translate 0 200 . smallText . show . score $ gs,
        smallText "Status: " . status $ gs,
        smallText "Elapsed time (s): " . msToSec . elapsedTime $ gs,
        smallText "Tick time (ms): " . tickTime $ gs,
        smallText "Lives: " . unlives . lives . player $ gs,
        smallText "Direction: " . direction $ gs,
        smallText "Buffer Direction: " . bufDirection $ gs,
        smallText "Position: " . getPosition . player $ gs,
        smallText "Coordinate decimals x, y: " $ show (formatDecimals x 1) ++ ", " ++ show (formatDecimals y 1),
        smallText "Can switch x, y: " $ (show . canMovePerpendicular $ x) ++ ", " ++ (show . canMovePerpendicular $ y)
      ]
  where
    smallText :: Show a => String -> a -> Picture
    smallText name a = color white . scale 0.1 0.1 . text $ name ++ show a

    stack :: Float -> Float -> Float -> [Picture] -> [Picture]
    stack _ _ _ [] = []
    stack x y pixels (l : ls) = translate x y l : stack x (y + pixels) pixels ls

    msToSec :: Time -> Float
    msToSec t = fromIntegral t / 1000

    (x, y) = getPosition . player $ gs

-- | Input handling
inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) IO.Down _ _) gs = movePlayer M.Up gs
inputHandler (EventKey (SpecialKey KeyDown) IO.Down _ _) gs = movePlayer M.Down gs
inputHandler (EventKey (SpecialKey KeyRight) IO.Down _ _) gs = movePlayer M.Right gs
inputHandler (EventKey (SpecialKey KeyLeft) IO.Down _ _) gs = movePlayer M.Left gs
inputHandler (EventKey (Char 'p') IO.Down _ _) gs = pause gs
inputHandler (EventKey (Char 'r') IO.Down _ _) gs = resume gs
inputHandler (EventKey (Char 'q') IO.Down _ _) gs = quit gs
inputHandler _ w = w

-- | Update function ran each iteration
updateFunc :: Float -> GameState -> GameState
updateFunc s gs = let ms = round (s * 1000) in step ms gs
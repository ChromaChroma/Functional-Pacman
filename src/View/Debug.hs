module View.Debug where

import Controller.MovementController
import Graphics.Gloss
import Model.Game
import Model.Level
import Model.Movement
import Model.Player
import View.Config
import View.Helpers
import Model.Items (PointItem(Dot, Fruit))

renderDebug :: GameState -> Picture
renderDebug gs = renderIfDebug $pictures [renderDebugDetails gs]

-- | Checks if application is in debug mode, if so render the passed Picture else return Blank
renderIfDebug :: Picture -> Picture
renderIfDebug f = if debug then f else blank

renderDebugDetails :: GameState -> Picture
renderDebugDetails gs =
  let (x, y) = getPosition . player $ gs
   in pictures . stack 25 $
        reverse
          [ smallText "Amount of dots: " . length $ [x | x@Dot {} <- items . level $ gs],
            smallText "No Fruit spawned: " . null $ ([x | x@Fruit {} <- items . level $ gs]),
            smallText "Status: " . status $ gs,
            smallText "Points: " . points $ gs,
            smallText "Elapsed time (s): " $ (fromIntegral . elapsedTime $ gs) / 1000,
            smallText "Tick time (ms): " . tickTimer $ gs,
            smallText "Lives: " . unlives . lives . player $ gs,
            smallText "Direction: " . direction . player $ gs,
            smallText "Buffer Direction: " . bufDirection . player $ gs,
            smallText "Position: " . getPosition . player $ gs,
            smallText "Coordinate decimals x, y: " $ show (formatDecimals x 1) ++ ", " ++ show (formatDecimals y 1),
            smallText "Can switch x, y: " $ (show . canMovePerpendicular $ x) ++ ", " ++ (show . canMovePerpendicular $ y),
            smallText "Intersections: " . levelIntersections . level $ gs,
            smallText "Player is colliding with ghost: " . isCollidingWithGhost $ gs,
            smallText "Player is colliding with item: " . isCollidingWithItem $ gs
          ]
  where
    isCollidingWithGhost gs = any (player gs `collides`) $ ghosts gs
    isCollidingWithItem gs = any (player gs `collides`) (items . level $ gs)


-- | Renders the intersections calculated by the game based on the level layout
renderIntersections :: GameState -> Picture
renderIntersections gs = renderIfDebug $ pictures [block (x, y) | (x, y) <- levelIntersections . level $ gs]
  where
    block (x, y) = color (dim green) . translateByTileSize (fromIntegral x) (fromIntegral y) $ rectangleSolid renderSize renderSize
    renderSize = tileSize / 2

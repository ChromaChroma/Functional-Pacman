module View.Debug where

import Controller.MovementController
import Graphics.Gloss
import Model.Game
import Model.Ghosts hiding (direction)
import Model.Items (PointItem (Dot, Fruit))
import Model.Level
import Model.Movement
import Model.Player
import Model.Utils
import View.Config
import View.Helpers

renderDebug :: Bool -> GameState -> Picture
renderDebug isDebug gs = renderIfDebug isDebug $ pictures [renderDebugDetails gs]

-- | Checks if application is in debug mode, if so render the passed Picture else return Blank
renderIfDebug :: Bool -> Picture -> Picture
renderIfDebug True p = p
renderIfDebug False _ = blank

renderDebugDetails :: GameState -> Picture
renderDebugDetails gs =
  let (x, y) = getPosition . player $ gs
   in pictures . stack 25 $
        reverse
          [ smallText "GhostState: " . ghostMode $ gs,
            smallText "Amount of dots: " . length $ [x | x@Dot {} <- items . level $ gs],
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
renderIntersections :: Bool -> GameState -> Picture
renderIntersections isDebug gs = renderIfDebug isDebug $ pictures [block (x, y) | (x, y) <- levelIntersections . level $ gs]
  where
    block (x, y) = color (dim green) . translateByTileSize (fromIntegral x) (fromIntegral y) $ rectangleSolid renderSize renderSize
    renderSize = tileSize / 2

module View.Helpers where

import Graphics.Gloss
import Model.Game
import View.Config

-- |
-- | Render Helper functions
-- |

-- | Translate a picture to the level section of the screen
translateToLevelSection :: Picture -> Picture
translateToLevelSection = translate 300 0

-- | Translate a picture by their coordinate on a level times the current tile size
translateByTileSize :: Float -> Float -> Picture -> Picture
translateByTileSize x y = translate (x * tileSize) (y * tileSize)

-- | Create a Text Picture with the given text and result of show `a` 
smallText :: Show a => String -> a -> Picture
smallText name a = color white . scale 0.1 0.1 . text $ name ++ show a

-- | Stacks a list of pictures vertically with the given pixels between each picture
stack :: Float -> [Picture] -> [Picture]
stack pxs = stack' 0
  where
    stack' _ [] = []
    stack' y (p:ps) = translate 0 y p : stack' (y + pxs) ps

-- | Convert the given milliseconds time to seconds
msToSec :: Time -> Float
msToSec t = fromIntegral t / 1000

module View.Helpers where

import Graphics.Gloss ( translate, pictures, Picture, white, color, scale, text, blank )
import Model.Game ( Time )
import View.Config ( tileSize )
import Data.List.Index (imap)

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

-- | Create a Text Picture with the text stacked `n` times with a 1 pixel offset between them 
layeredText :: Float -> Float -> Int -> String -> Picture
layeredText x y n txt = pictures $ map (\i -> translate (x+i) (y-1) . color white $ text txt) [1..fromIntegral n] 

-- | Stacks a list of pictures vertically with the given pixels between each picture
stack :: Float -> [Picture] -> [Picture]
stack pxs = stack' 0
  where
    stack' _ [] = []
    stack' y (p:ps) = translate 0 y p : stack' (y + pxs) ps

-- | Convert the given milliseconds time to seconds
msToSec :: Time -> Float
msToSec t = fromIntegral t / 1000

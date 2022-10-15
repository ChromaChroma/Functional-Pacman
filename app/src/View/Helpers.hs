module View.Helpers where

import Data.List.Index (imap)
import Graphics.Gloss (Picture, blank, color, pictures, scale, text, translate, white)
import Model.Game (Time)
import Model.Level (LevelSize)
import View.Config (tileSize, windowSize)

-- |
-- | Render Helper functions
-- |

-- | Translate a picture to the level section of the screen to the center, based on the level size and screen size
translateToLevelSection :: LevelSize -> Picture -> Picture
translateToLevelSection (lw, lh) = translate (sw' - lw') (sh' - lh')
  where
    (lw', lh') = (fromIntegral lw /2 * tileSize, fromIntegral lh / 2 * tileSize)
    (sw', sh') = (fromIntegral sw / 2, fromIntegral sh / 2)
    (sw, sh) = windowSize

-- | Translate a picture to the info section of the screen above the level, based on the level size and screen size
translateToAboveLevelSection :: LevelSize -> Picture -> Picture
translateToAboveLevelSection (lw, lh) = translate (sw' - lw') (sh' + lh')
  where
    (lw', lh') = (fromIntegral lw /2 * tileSize, fromIntegral lh / 2 * tileSize)
    (sw', sh') = (fromIntegral sw / 2, fromIntegral sh / 2)
    (sw, sh) = windowSize

-- | Translate a picture by their coordinate on a level times the current tile size
translateByTileSize :: Float -> Float -> Picture -> Picture
translateByTileSize x y = translate (x * tileSize) (y * tileSize)

-- | Create a Text Picture with the given text and result of show `a`
smallText :: Show a => String -> a -> Picture
smallText name a = color white . scale 0.1 0.1 . text $ name ++ show a

-- | Create a Text Picture with the text stacked `n` times with a 1 pixel offset between them
layeredText :: Float -> Float -> Int -> String -> Picture
layeredText x y n txt = pictures $ map (\i -> translate (x + i) (y -1) . color white $ text txt) [1 .. fromIntegral n]

-- | Stacks a list of pictures vertically with the given pixels between each picture
stack :: Float -> [Picture] -> [Picture]
stack pxs = stack' 0
  where
    stack' _ [] = []
    stack' y (p : ps) = translate 0 y p : stack' (y + pxs) ps

-- | Convert the given milliseconds time to seconds
msToSec :: Time -> Float
msToSec t = fromIntegral t / 1000

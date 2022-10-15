module View.Animation where

import Graphics.Gloss
import View.Config(tileSize)
import Control.Applicative ((<*>), (<$>))

data Textures = Textures { pacman :: Animation }
data Animation = Animation { currentFrame :: Int, frames :: [Picture] }

loadTextures :: IO Textures
loadTextures = do
  pacmanAnimation <- Animation 0  <$> mapM loadBMP ["assets/pacman-1.bmp", "assets/pacman-2.bmp", "assets/pacman-3.bmp"]
  
  return $ Textures pacmanAnimation

pacMan :: Textures -> Picture
pacMan ts = (frames . pacman $ ts) !! 0
-- pacMan ts = color yellow . circleSolid $ tileSize / 2

module View.Animation where

import Graphics.Gloss
import Control.Applicative ((<*>), (<$>))
import View.Config(tileSize)
import Model.Movement as M

data Textures = Textures { pacman :: Animation }
data Animation = Animation { currentFrame :: Int, frames :: [Picture] }

loadTextures :: IO Textures
loadTextures = do
  pacmanAnimation <- Animation 0  <$> mapM loadBMP ["assets/pacman-1.bmp", "assets/pacman-2.bmp", "assets/pacman-3.bmp"]
  
  return $ Textures pacmanAnimation

pacMan :: Textures -> Direction  -> Picture
pacMan ts dir = rotate (dirRotation dir) $ (frames . pacman $ ts) !! 0

dirRotation :: Direction  -> Float
dirRotation M.Right = 0
dirRotation M.Down = 90
dirRotation M.Left = 180
dirRotation M.Up = 270
dirRotation M.Stop = 270

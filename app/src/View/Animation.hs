module View.Animation where

import Data.Fixed
import Control.Applicative ((<$>), (<*>))
import Graphics.Gloss
import Model.Game hiding (elapsedTime)
import Model.Movement as M

type FramesPerSecond = Float

data Textures = Textures
  { elapsedTime :: Float, -- In seconds
    pacman :: Animation
  }

data Animation = Animation {framesPerSecond :: FramesPerSecond, frames :: [Picture]}

-- | Function for the initial texture loading. It is called once at the beginning of the game.
loadTextures :: IO Textures
loadTextures = do
  pacmanAnimation <- Animation 12 <$> mapM loadBMP ["assets/pacman-1.bmp", "assets/pacman-2.bmp", "assets/pacman-3.bmp"]
  return $ Textures 0 pacmanAnimation

-- | Function for the texture updating. It is called every frame.
pacMan :: Textures -> Direction -> Picture
pacMan ts dir = rotate (dirRotation dir) (frms !! frameNr) 
  where
    frameNr = getFrameNumber anim (elapsedTime ts)
    frms = frames anim
    anim = pacman ts

-- | Function to rotate picture based on the movement direction
dirRotation :: Direction -> Float
dirRotation M.Right = 0
dirRotation M.Down = 90
dirRotation M.Left = 180
dirRotation M.Up = 270
dirRotation M.Stop = 270

-- | Function to get the current frame number based on the elapsed time and the frames per second of the animation
getFrameNumber :: Animation -> Float -> Int
getFrameNumber anim eT = floor $ (eT * fps) `mod'` frms
  where
    fps = framesPerSecond anim
    frms = fromIntegral . length $ frames anim
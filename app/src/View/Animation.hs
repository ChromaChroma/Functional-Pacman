module View.Animation where

import Control.Applicative ((<$>), (<*>))
import Data.Fixed (mod')
import Graphics.Gloss (Picture, loadBMP, rotate)
import Model.Game ()
import Model.Movement as M (Direction (..))
import qualified View.Config hiding (fps)

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | Frames per second of an animation
type FramesPerSecond = Float

-- | Elapsed time in seconds
type ElapsedTime = Float

-- | Data structure that contains all the texture and animation data
data Textures = Textures
  { elapsedTime :: ElapsedTime,
    pacman :: Animation
  }

data Animation = Animation {fps :: FramesPerSecond, frames :: [Picture]}

-------------------------------------------------------------------------------
-- Impure, initial texture loading function
-------------------------------------------------------------------------------

-- | Function for the initial texture loading. It is called once at the beginning of the game.
loadTextures :: IO Textures
loadTextures = do
  pacmanAnimation <- Animation 12 <$> mapM loadBMP ["assets/pacman-1.bmp", "assets/pacman-2.bmp", "assets/pacman-3.bmp"]
  return Textures {elapsedTime = 0, pacman = pacmanAnimation}

-------------------------------------------------------------------------------
-- Pure, generic texture loading functions
-------------------------------------------------------------------------------

-- | Loads a rotated version of the current frame of the animation based on the direction.
loadAnimationFrameInDirection :: Animation -> ElapsedTime -> Direction -> Picture
loadAnimationFrameInDirection anim elapsedTime dir = rotate (dirRotation dir) (loadAnimationFrame anim elapsedTime)

-- | Loads the current frame of the animation based on the elapsed time.
loadAnimationFrame :: Animation -> ElapsedTime -> Picture
loadAnimationFrame anim elapsedTime = frames anim !! getFrameNumber anim elapsedTime

-- | Function to rotate picture based on the movement direction
dirRotation :: Direction -> Float
dirRotation M.Right = 0
dirRotation M.Down = 90
dirRotation M.Left = 180
dirRotation M.Up = 270
dirRotation M.Stop = 270

-------------------------------------------------------------------------------
-- Pure, specific texture (Picture) loading functions
-------------------------------------------------------------------------------

-- | Function for the texture updating. It is called every frame.
pacMan :: Textures -> Direction -> Picture
pacMan ts = loadAnimationFrameInDirection (pacman ts) (elapsedTime ts)

-- | Function to get the current frame number based on the elapsed time and the frames per second of the animation
getFrameNumber :: Animation -> ElapsedTime -> Int
getFrameNumber anim eT = floor $ (eT * fps anim) `mod'` totalFrames
  where
    totalFrames = fromIntegral . length $ frames anim

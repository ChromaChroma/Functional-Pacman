module View.Animation (Textures (..), loadTextures, pacMan, ghost) where

import Control.Applicative ((<$>), (<*>))
import Data.Fixed (mod')
import Graphics.Gloss (Picture, loadBMP, rotate)
import Model.Game ()
import Model.Ghosts (Ghost (direction, lifeState, mode, name), GhostState (Frightened), LifeState (Alive, Eaten), Name (..))
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
    pacman :: Animation,
    blinky :: DirectionalAnimation,
    pinky :: DirectionalAnimation,
    inky :: DirectionalAnimation,
    clyde :: DirectionalAnimation,
    ghostFrightened :: Animation,
    ghostFrightenedFlashing :: Animation,
    ghostEaten :: DirectionalAnimation
  }

data Animation = Animation {fps :: FramesPerSecond, frames :: [Picture]}

data DirectionalAnimation = DirectionalAnimation {left :: Animation, right :: Animation, up :: Animation, down :: Animation}

-------------------------------------------------------------------------------
-- Impure, initial texture loading function
-------------------------------------------------------------------------------

-- | Function for the initial texture loading. It is called once at the beginning of the game.
loadTextures :: IO Textures
loadTextures = do
  pacmanAnimation <- Animation 9 <$> mapM loadBMP ["assets/pacman-1.bmp", "assets/pacman-2.bmp", "assets/pacman-3.bmp"]
  ghostFrigthenedAnimation <- Animation 6 <$> mapM loadBMP ["assets/ghost-frightened-1.bmp", "assets/ghost-frightened-2.bmp"]
  ghostFrigthenedFlashingAnimation <- Animation 6 <$> mapM loadBMP ["assets/ghost-frightened-flashing-1.bmp", "assets/ghost-frightened-flashing-2.bmp"]
  blinkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/blinky-left-1.bmp", "assets/blinky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/blinky-right-1.bmp", "assets/blinky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/blinky-up-1.bmp", "assets/blinky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/blinky-down-1.bmp", "assets/blinky-down-2.bmp"])
  pinkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/pinky-left-1.bmp", "assets/pinky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/pinky-right-1.bmp", "assets/pinky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/pinky-up-1.bmp", "assets/pinky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/pinky-down-1.bmp", "assets/pinky-down-2.bmp"])
  inkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/inky-left-1.bmp", "assets/inky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/inky-right-1.bmp", "assets/inky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/inky-up-1.bmp", "assets/inky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/inky-down-1.bmp", "assets/inky-down-2.bmp"])
  clydeAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/clyde-left-1.bmp", "assets/clyde-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/clyde-right-1.bmp", "assets/clyde-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/clyde-up-1.bmp", "assets/clyde-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/clyde-down-1.bmp", "assets/clyde-down-2.bmp"])
  ghostEatenAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghost-eaten-left.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghost-eaten-right.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghost-eaten-up.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghost-eaten-down.bmp"])
  return
    Textures
      { elapsedTime = 0,
        pacman = pacmanAnimation,
        blinky = blinkyAnimation,
        pinky = pinkyAnimation,
        inky = inkyAnimation,
        clyde = clydeAnimation,
        ghostFrightened = ghostFrigthenedAnimation,
        ghostFrightenedFlashing = ghostFrigthenedFlashingAnimation,
        ghostEaten = ghostEatenAnimation
      }

-------------------------------------------------------------------------------
-- Pure, generic texture loading functions
-------------------------------------------------------------------------------

-- | Loads the current frame of the animation of the movement direction.
loadAnimationFrameInDirection :: DirectionalAnimation -> ElapsedTime -> Direction -> Picture
loadAnimationFrameInDirection dAnim elapsedTime dir = case dir of
  M.Right -> loadAnimationFrame (right dAnim) elapsedTime
  M.Down -> loadAnimationFrame (down dAnim) elapsedTime
  M.Left -> loadAnimationFrame (left dAnim) elapsedTime
  M.Up -> loadAnimationFrame (up dAnim) elapsedTime
  M.Stop -> loadAnimationFrame (right dAnim) elapsedTime

-- | Loads the rotated version of the current frame of the animation based on the direction.
loadAnimationFrameRotated :: Animation -> ElapsedTime -> Direction -> Picture
loadAnimationFrameRotated anim elapsedTime dir = rotate (dirRotation dir) (loadAnimationFrame anim elapsedTime)

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

-- | Function to get the current frame number based on the elapsed time and the frames per second of the animation
getFrameNumber :: Animation -> ElapsedTime -> Int
getFrameNumber anim eT = floor $ (eT * fps anim) `mod'` totalFrames
  where
    totalFrames = fromIntegral . length $ frames anim

-------------------------------------------------------------------------------
-- Pure, specific texture (Picture) loading functions
-------------------------------------------------------------------------------

-- | Function to get the PacMan animation frame
pacMan :: Textures -> Direction -> Picture
pacMan ts = loadAnimationFrameRotated (pacman ts) (elapsedTime ts)

-- | Function to get the ghost animation frame, based on the ghost name, mode and alive state
ghost :: Textures -> Ghost -> Picture
ghost ts g =
  if lifeState g == Alive && mode g == Frightened
    then loadAnimationFrame (ghostFrightened ts) (elapsedTime ts)
    else getGhostAnimation ts g

-- | Function to get the ghost animation frame
getGhostAnimation :: Textures -> Ghost -> Picture
getGhostAnimation ts g = case lifeState g of
  Eaten -> loadAnimationFrameInDirection (ghostEaten ts) (elapsedTime ts) dir
  _ -> case name g of
    Blinky -> loadAnimationFrameInDirection (blinky ts) (elapsedTime ts) dir
    Pinky -> loadAnimationFrameInDirection (pinky ts) (elapsedTime ts) dir
    Inky -> loadAnimationFrameInDirection (inky ts) (elapsedTime ts) dir
    Clyde -> loadAnimationFrameInDirection (clyde ts) (elapsedTime ts) dir
  where
    dir = direction g

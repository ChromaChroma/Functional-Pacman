module View.Animation
  ( Textures (..),
    Texture,
    TileTextures (..),
    loadTextures,
    pacMan,
    ghost,
    fruitTexture,
  )
where

import Control.Applicative ((<$>), (<*>))
import Data.Fixed (mod')
import Graphics.Gloss (Picture, circleSolid, color, loadBMP, red, rotate)
import Model.Game (GhostMode (Frightened), Time, frightenedDuration)
import Model.Ghosts (EatenState (Eaten, NotEaten), Ghost (direction, name), Name (..), isEaten, isNotEaten)
import Model.Items (FruitType (..), PointItem (Fruit, itemType))
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
    pacmanLife :: Texture,
    pacman :: Animation,
    blinky :: DirectionalAnimation,
    pinky :: DirectionalAnimation,
    inky :: DirectionalAnimation,
    clyde :: DirectionalAnimation,
    ghostFrightened :: Animation,
    ghostFrightenedFlashing :: Animation,
    ghostEaten :: DirectionalAnimation,
    fruits :: FruitTextures,
    tileTextures :: TileTextures
  }

-- | Type alias of a static texture
type Texture = Picture

-- | Data structure that contains the frames of an animation
data Animation = Animation {fps :: FramesPerSecond, frames :: [Picture]}

-- | Data structure that contains animations for each direction
data DirectionalAnimation = DirectionalAnimation {left :: Animation, right :: Animation, up :: Animation, down :: Animation}

-- | Data structure that contains the current the textures of each fruit
data FruitTextures = FruitTextures
  { cherry :: Texture,
    strawberry :: Texture,
    orange :: Texture,
    apple :: Texture,
    melon :: Texture,
    galaxian :: Texture,
    bell :: Texture,
    key :: Texture
  }

data TileTextures = TileTextures
  { endSingle :: Texture,
    straight :: Texture,
    straightSingle :: Texture,
    corner :: Texture,
    cornerSingle :: Texture,
    cornerSingleToDouble :: Texture,
    cornerSingleToDoubleMirrored :: Texture,
    crossSectionSingle :: Texture,
    crossSectionFishShaped :: Texture,
    tJunction :: Texture,
    tJunctionSingle :: Texture,
    surroundedWall :: Texture,
    ghostDoorStraight :: Texture,
    ghostDoorCorner :: Texture,
    missingTexture :: Texture
  }

-------------------------------------------------------------------------------
-- Impure, initial texture loading function
-------------------------------------------------------------------------------

-- | Function for the initial texture loading. It is called once at the beginning of the game.
loadTextures :: IO Textures
loadTextures = do
  pacmanLife <- loadBMP "assets/player/pacman-2.bmp"
  pacmanAnimation <- Animation 9 <$> mapM loadBMP ["assets/player/pacman-1.bmp", "assets/player/pacman-2.bmp", "assets/player/pacman-3.bmp"]
  ghostFrigthenedAnimation <- Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-frightened-1.bmp", "assets/ghosts/ghost-frightened-2.bmp"]
  ghostFrigthenedFlashingAnimation <- Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-frightened-1.bmp", "assets/ghosts/ghost-frightened-2.bmp", "assets/ghosts/ghost-frightened-flashing-1.bmp", "assets/ghosts/ghost-frightened-flashing-2.bmp"]
  blinkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghosts/blinky-left-1.bmp", "assets/ghosts/blinky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/blinky-right-1.bmp", "assets/ghosts/blinky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/blinky-up-1.bmp", "assets/ghosts/blinky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/blinky-down-1.bmp", "assets/ghosts/blinky-down-2.bmp"])
  pinkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghosts/pinky-left-1.bmp", "assets/ghosts/pinky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/pinky-right-1.bmp", "assets/ghosts/pinky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/pinky-up-1.bmp", "assets/ghosts/pinky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/pinky-down-1.bmp", "assets/ghosts/pinky-down-2.bmp"])
  inkyAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghosts/inky-left-1.bmp", "assets/ghosts/inky-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/inky-right-1.bmp", "assets/ghosts/inky-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/inky-up-1.bmp", "assets/ghosts/inky-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/inky-down-1.bmp", "assets/ghosts/inky-down-2.bmp"])
  clydeAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghosts/clyde-left-1.bmp", "assets/ghosts/clyde-left-2.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/clyde-right-1.bmp", "assets/ghosts/clyde-right-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/clyde-up-1.bmp", "assets/ghosts/clyde-up-2.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/clyde-down-1.bmp", "assets/ghosts/clyde-down-2.bmp"])
  ghostEatenAnimation <-
    DirectionalAnimation . Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-eaten-left.bmp"]
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-eaten-right.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-eaten-up.bmp"])
      <*> (Animation 6 <$> mapM loadBMP ["assets/ghosts/ghost-eaten-down.bmp"])
  fruits <-
    FruitTextures
      <$> loadBMP "assets/fruits/cherry.bmp"
      <*> loadBMP "assets/fruits/strawberry.bmp"
      <*> loadBMP "assets/fruits/orange.bmp"
      <*> loadBMP "assets/fruits/apple.bmp"
      <*> loadBMP "assets/fruits/melon.bmp"
      <*> loadBMP "assets/fruits/galaxian.bmp"
      <*> loadBMP "assets/fruits/bell.bmp"
      <*> loadBMP "assets/fruits/key.bmp"
  tileTextures <-
    TileTextures
      <$> loadBMP "assets/tiles/end-single.bmp"
      <*> loadBMP "assets/tiles/straight.bmp"
      <*> loadBMP "assets/tiles/straight-single.bmp"
      <*> loadBMP "assets/tiles/corner.bmp"
      <*> loadBMP "assets/tiles/corner-single.bmp"
      <*> loadBMP "assets/tiles/corner-single-to-double.bmp"
      <*> loadBMP "assets/tiles/corner-single-to-double-mirrored.bmp"
      <*> loadBMP "assets/tiles/cross-section-single.bmp"
      <*> loadBMP "assets/tiles/cross-section-fish-shaped.bmp"
      <*> loadBMP "assets/tiles/t-junction.bmp"
      <*> loadBMP "assets/tiles/t-junction-single.bmp"
      <*> loadBMP "assets/tiles/surrounded-wall.bmp"
      <*> loadBMP "assets/tiles/ghost-door-straight.bmp"
      <*> loadBMP "assets/tiles/ghost-door-corner.bmp"
      <*> loadBMP "assets/tiles/dev.bmp"
  return
    Textures
      { elapsedTime = 0,
        pacmanLife = pacmanLife,
        pacman = pacmanAnimation,
        blinky = blinkyAnimation,
        pinky = pinkyAnimation,
        inky = inkyAnimation,
        clyde = clydeAnimation,
        ghostFrightened = ghostFrigthenedAnimation,
        ghostFrightenedFlashing = ghostFrigthenedFlashingAnimation,
        ghostEaten = ghostEatenAnimation,
        fruits = fruits,
        tileTextures = tileTextures
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
ghost :: Textures -> GhostMode -> Time -> Ghost -> Picture
ghost ts gState frightenedTime g
  | isNotEatenAndFrightened && frightenedTime > (frightenedDuration - 2000) = loadAnimationFrame (ghostFrightenedFlashing ts) (elapsedTime ts)
  | isNotEatenAndFrightened = loadAnimationFrame (ghostFrightened ts) (elapsedTime ts)
  | otherwise = getGhostAnimation ts g
  where
    isNotEatenAndFrightened = isNotEaten g && gState == Frightened

-- | Function to get the ghost animation frame
getGhostAnimation :: Textures -> Ghost -> Picture
getGhostAnimation ts g =
  if isEaten g
    then loadAnimationFrameInDirection (ghostEaten ts) (elapsedTime ts) dir
    else case name g of
      Blinky -> loadAnimationFrameInDirection (blinky ts) (elapsedTime ts) dir
      Pinky -> loadAnimationFrameInDirection (pinky ts) (elapsedTime ts) dir
      Inky -> loadAnimationFrameInDirection (inky ts) (elapsedTime ts) dir
      Clyde -> loadAnimationFrameInDirection (clyde ts) (elapsedTime ts) dir
  where
    dir = direction g

fruitTexture :: Textures -> PointItem -> Picture
fruitTexture ts Fruit {itemType = t} = case t of
  Cherry -> cherry . fruits $ ts
  Strawberry -> strawberry . fruits $ ts
  Orange -> orange . fruits $ ts
  Apple -> apple . fruits $ ts
  Melon -> melon . fruits $ ts
  Galaxian -> galaxian . fruits $ ts
  Bell -> bell . fruits $ ts
  Key -> key . fruits $ ts
fruitTexture _ _ = error "Not a fruit"

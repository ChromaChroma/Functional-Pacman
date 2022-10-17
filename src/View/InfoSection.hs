module View.InfoSection (renderInfoSection) where

import Graphics.Gloss (Picture (Blank), color, pictures, polygon, rectangleSolid, rose, translate)
import Model.Game (GameState (elapsedTime, level, player, points), Time)
import Model.Level (Level (layout), layoutSize)
import Model.Player (Lives (Lives, unlives), Player (lives))
import Model.Score (Points, Score)
import View.Animation (Textures (pacmanLife))
import View.Config (tileSize)
import View.Helpers (smallText, smallTextOnly, translateToAboveLevelSection)

renderInfoSection :: Textures -> GameState -> Picture
renderInfoSection textures gs =
  translateToAboveLevelSection (layoutSize . layout . level $ gs) $
    pictures
      [ renderLives textures . lives . player $ gs,
        translate 150 0 . renderTime . elapsedTime $gs,
        translate 400 0 . renderScore . points $ gs,
        translate 600 0 . renderHighScore $ gs
      ]

-- | Render the players lives, if lives is bigger than 3, render a life picture with the number of lives
renderLives :: Textures -> Lives -> Picture
renderLives textures (Lives lives)
  | lives <= 3 = pictures [lifePictures]
  | otherwise = pictures [life 1, translate (tileSize / 1.5) 0 . smallTextOnly . show $ lives]
  where
    lifePictures = pictures . map life $ [1 .. (fromIntegral lives)]
    life n = translate ((n -1) * tileSize) (tileSize / 5) $ pacmanLife textures

renderTime :: Time -> Picture
renderTime t = smallText "Elapsed time: " . floor $ fromIntegral t / 1000

renderScore :: Points -> Picture
renderScore = smallText "Score: "

renderHighScore :: GameState -> Picture
renderHighScore gs = Blank

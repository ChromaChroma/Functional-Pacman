module View.Overlays (renderOverlay) where

import Graphics.Gloss (Picture, blank, color, rectangleSolid, pictures,)
import Graphics.Gloss.Data.Color (makeColorI)
import Model.Game (GameState (status), Status (Paused, GameOver))
import View.Config (windowSize)
import View.Helpers ( layeredText )

renderOverlay :: GameState -> Picture
renderOverlay gs
  | status gs == Paused = renderPauseOverlay
  | status gs == GameOver = renderGameOver
  | otherwise = blank

renderPauseOverlay :: Picture
renderPauseOverlay = pictures [overlayBackdrop, layeredText (-210) (-45) 5 "Paused"]

renderGameOver :: Picture
renderGameOver = pictures [overlayBackdrop, layeredText (-250) (-45) 5 "Game Over"]
-- Todo make this: renderGameOver overlayBackdropText "Game Over";; Could pass n times text

-- todo: convert to text on overlay function that takes a string and calculates the translation shift, and backdrop witdth
overlayBackdrop :: Picture
overlayBackdrop = pictures [background, textBackdrop] 
  where
    (w, h) = windowSize 
    background = color (makeColorI 0 0 0 150) $ rectangleSolid (fromIntegral w) (fromIntegral h)
    textBackdrop = color (makeColorI 0 0 0 240) $ rectangleSolid (fromIntegral w) (fromIntegral h /3)

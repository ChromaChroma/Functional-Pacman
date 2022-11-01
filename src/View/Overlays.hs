module View.Overlays (renderOverlay) where

import Graphics.Gloss (Picture, blank, color, pictures, rectangleSolid)
import Graphics.Gloss.Data.Color (makeColorI)
import Model.Game (GameState (status), Status (GameOver, Paused))
import View.Config (windowSize)
import View.Helpers (layeredText, smallTextOnly, stack)
import Model.Game
import Model.Score

renderOverlay :: GameState -> Picture
renderOverlay gs
  | status gs == Paused = renderPauseOverlay
  | status gs == GameOver = renderGameOver gs
  | otherwise = blank

renderPauseOverlay :: Picture
renderPauseOverlay = pictures [overlayBackdrop, layeredText (-210) (-45) 5 "Paused"]

renderGameOver :: GameState -> Picture
renderGameOver gs =
  pictures
    [ overlayBackdrop,
      layeredText (-250) (-45) 5 "Game Over",
      pictures . stack 20 $ map (\s -> smallTextOnly $ show s) (take 10 . getScores $ highScores gs)
    ]

-- Todo make this: renderGameOver overlayBackdropText "Game Over";; Could pass n times text

-- todo: convert to text on overlay function that takes a string and calculates the translation shift, and backdrop witdth
overlayBackdrop :: Picture
overlayBackdrop = pictures [background, textBackdrop]
  where
    (w, h) = windowSize
    background = color (makeColorI 0 0 0 150) $ rectangleSolid (fromIntegral w) (fromIntegral h)
    textBackdrop = color (makeColorI 0 0 0 240) $ rectangleSolid (fromIntegral w) (fromIntegral h / 3)

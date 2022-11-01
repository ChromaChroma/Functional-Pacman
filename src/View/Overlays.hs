module View.Overlays (renderOverlay) where

import Graphics.Gloss (Picture, blank, color, pictures, rectangleSolid, scale)
import Graphics.Gloss.Data.Color (makeColorI)
import Model.Game (GameState (status), Status (GameOver, Paused))
import View.Config (windowSize)
import View.Helpers (layeredText, smallTextOnly, stack)
import Model.Game
import Model.Score
import Data.List(foldl')

renderOverlay :: GameState -> String -> Picture
renderOverlay gs textBuffer
  | status gs == Paused = renderPauseOverlay
  | status gs == GameOver = renderGameOver gs textBuffer
  | otherwise = blank

renderPauseOverlay :: Picture
renderPauseOverlay = pictures [overlayBackdrop, layeredText (-210) (-45) 5 "Paused"]

renderGameOver :: GameState -> String -> Picture
renderGameOver gs textBuffer =
  let scores = getScores $ highScores gs
  in pictures
    [ overlayBackdrop,
      layeredText (-250) (-45) 5 "Game Over",
      scale 0.3 0.3 . layeredText (-500) (-300) 5 $ "Enter Name: " ++ textBuffer,
      pictures . stack 20 $ map (\s -> smallTextOnly $ show s) (foldl' (const . drop 1) scores (drop 10 scores))
    ]

-- Todo make this: renderGameOver overlayBackdropText "Game Over";; Could pass n times text

-- todo: convert to text on overlay function that takes a string and calculates the translation shift, and backdrop witdth
overlayBackdrop :: Picture
overlayBackdrop = pictures [background, textBackdrop]
  where
    (w, h) = windowSize
    background = color (makeColorI 0 0 0 150) $ rectangleSolid (fromIntegral w) (fromIntegral h)
    textBackdrop = color (makeColorI 0 0 0 240) $ rectangleSolid (fromIntegral w) (fromIntegral h / 3)

module View.Overlays (renderOverlay) where

import Data.List (foldl')
import Graphics.Gloss (Picture, blank, color, pictures, rectangleSolid, scale, translate)
import Graphics.Gloss.Data.Color (makeColorI)
import Model.Game
import Model.Game (GameState (status), Status (GameOver, Paused))
import Model.Score
import View.Config (windowSize)
import View.Helpers (layeredText, smallTextOnly, stack)

renderOverlay :: GameState -> String -> Picture
renderOverlay gs textBuffer
  | status gs == Waiting = renderWaitingOverlay gs
  | status gs == Paused = renderPauseOverlay gs
  | status gs == GameOver = renderGameOver gs textBuffer
  | otherwise = blank

renderPauseOverlay :: GameState -> Picture
renderPauseOverlay gs =
  pictures
    [ overlayBackdrop gs,
      translate (-20) 0 $ layeredText (-210) (-45) 5 "Paused",
      translate (-150) (-100) . scale 0.3 0.3 $ layeredText (-210) (-100) 5 "Press 'r' to continue..."
    ]

renderWaitingOverlay :: GameState -> Picture
renderWaitingOverlay gs =
  pictures
    [ overlayBackdrop gs,
      translate (-50) 0 . scale 0.3 0.3 $ layeredText (-600) (-45) 5 "Press SPACE to start..."
    ]

renderGameOver :: GameState -> String -> Picture
renderGameOver gs textBuffer =
  pictures
    [ overlayBackdrop gs,
      translate (-100) 0 $ layeredText (-250) (-45) 5 "Game Over",
      translate (50) (-10) . scale 0.3 0.3 $ layeredText (-500) (-300) 5 "Enter Name: ", 
      translate (fromIntegral $ 170 - (length textBuffer * 10)) (-70) . scale 0.3 0.3 $ layeredText (-500) (-300) 12 textBuffer,
      translate (-200) (-150) . scale 0.2 0.2 . layeredText (-500) (-300) 5 $ "DEL = remove last character - ENTER = submit"
    ]

overlayBackdrop :: GameState -> Picture
overlayBackdrop gs =
  let scores = getScores $ highScores gs
   in pictures
        [ background,
          textBackdrop,
          translate (-50) 100 . pictures . stack 20 $ map (\s -> smallTextOnly $ show s) (foldl' (const . drop 1) scores (drop 10 scores))
        ]
  where
    (w, h) = windowSize
    background = color (makeColorI 0 0 0 150) $ rectangleSolid (fromIntegral w) (fromIntegral h)
    textBackdrop = color (makeColorI 0 0 0 240) $ rectangleSolid (fromIntegral w) (fromIntegral h / 2)

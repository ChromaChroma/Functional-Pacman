module View.InfoSection (renderInfoSection) where

import Graphics.Gloss
import View.Helpers
import Model.Game
import Model.Player
import Model.Score

renderInfoSection :: GameState -> Picture
renderInfoSection gs =
  pictures
    [ 
      -- renderScore . score $ gs,
      renderLives . lives . player $ gs,
      renderTime . elapsedTime $gs
    ]

renderLives :: Lives -> Picture
renderLives x = Blank

renderScore :: Score -> Picture
renderScore x = Blank

renderTime :: Time -> Picture
renderTime x = Blank

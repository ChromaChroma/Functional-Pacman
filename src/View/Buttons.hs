module View.Buttons where

import Graphics.Gloss
import Model.Game hiding (elapsedTime)
import View.Animation
import View.Config

-------------------------------------------------------------------------------
-- Data Structures
-------------------------------------------------------------------------------

type Position = (Float, Float)

type Size = (Float, Float)

data Button a = Button
  { position :: Position,
    size :: Size,
    label :: String,
    action :: a -> IO a
  }

-------------------------------------------------------------------------------
-- Type Class Implementations
-------------------------------------------------------------------------------

instance Show (Button a) where
  show b =
    "Button {position:" ++ (show $ position b)
      ++ ", size: "
      ++ (show $ size b)
      ++ ", label: "
      ++ (label b)
      ++ ", action: ...()}"

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

click :: Button a -> a -> IO a
click Button{action = a} = a

updateLabel :: String -> Button a -> Button a
updateLabel newLabel button = button {label = newLabel}

isClicked :: Button a -> Position -> Bool
isClicked b (xPos, yPos) =
  xPos >= x
    && xPos <= x'
    && yPos >= y
    && yPos <= y'
  where
    (x, y) = position b

    (w, h) = size b
    x' = x + w
    y' = y + h

-------------------------------------------------------------------------------
-- Render Functions
-------------------------------------------------------------------------------

drawButton :: Button a -> Picture
drawButton b =
  let (x, y) = position b
      (w, h) = size b
      lbl = color red . scale 0.1 0.1 $ text $ label b
   in translate x y $
        pictures
          [ translate ((w / 2) - 12) ((h / 2) - 13) . color white $ rectangleSolid w h,
            lbl
          ]

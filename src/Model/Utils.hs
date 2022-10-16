module Model.Utils where

import qualified Data.Fixed as DF ( mod' )

-- | Util function that calculates the modulo of a float, wrapping `a` back around to `b` if `a` is negative
-- | The function always returns a positive number or 0
-- | Example: 
-- |    (-1) `mod'` 10 == 9 (because -1 + 10 == 9) 
-- |     (1) `mod'` 10 == 1
mod' :: Float -> Float -> Float
mod' _ 0 = error "Cannot mod by 0"
mod' a b 
  | a < 0 = (a + b) `mod'` b
  | otherwise = a `DF.mod'` b
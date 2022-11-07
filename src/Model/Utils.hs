module Model.Utils where

import qualified Data.Fixed as DF (mod')
import Numeric (showFFloat)

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

intmod' :: Int -> Int -> Int
intmod' _ 0 = error "Cannot mod by 0"
intmod' a b
  | fromIntegral a < 0 = round $ (fromIntegral a + fromIntegral b) `mod'` fromIntegral b
  | otherwise = round $ fromIntegral a `DF.mod'` fromIntegral b

-- | Calculates the distance between two points
dist :: Num a => (a, a) -> (a, a) -> a
dist (x, y) (x', y') = abs (x - x') + abs (y - y')

-- | Takes init of string or [] if empty string
safeInit :: String -> String
safeInit [] = []
safeInit s = init s

-- | Takes float `n` and returns a float of the decimal places rounded to `j` decimals
-- | i.e formatDecimals 9.005 1 = 0.0, formatDecimals 9.05 1 = 0.1
formatDecimals :: RealFloat a => a -> Int -> Float
formatDecimals n j = read (showFFloat (Just j) (n `DF.mod'` 1) "") :: Float

-- | imap ~ indexedMapping 
-- | Is similar to the normal map function, but also takes the index `i` of `a` in the list and applies those to the mapped function
imap :: (Int -> a -> b) -> [a] -> [b]
imap f ls = map (uncurry f) (zip [0..] ls)
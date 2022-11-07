{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Score
  ( HighScores,
    Score (..),
    Points,
    firstPlace,
    getScores,
    add,
    mkScore,
  )
where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List (sort)
import GHC.Generics

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

type Points = Int

data Score = Score {name :: String, score :: Points} deriving (Generic, Show)

-- | Collection of all scores of the game
newtype HighScores = HighScores [Score] deriving (Eq, Generic, Show)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

-- Implementation of Eq and Ord for Score

instance Eq Score where
  Score _ scoreA == Score _ scoreB = scoreA == scoreB

instance Ord Score where
  compare x y = compare (score x) (score y)

-- | Automatic JSON encoding/decoding implementation
instance FromJSON Score

instance ToJSON Score

instance FromJSON HighScores

instance ToJSON HighScores

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

mkScore :: String -> Int -> Maybe Score
mkScore name score
  | score < 0 = Nothing
  | otherwise = Just Score {name = name, score = score}

add :: Score -> HighScores -> HighScores
add score (HighScores scores) = (score : scores)

firstPlace :: HighScores -> Maybe Score
firstPlace (HighScores []) = Nothing
firstPlace (HighScores scores) = Just (maximum scores)

getScores :: HighScores -> [Score]
getScores (HighScores scores) = sort scores
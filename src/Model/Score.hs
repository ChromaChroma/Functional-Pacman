{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Score
  ( HighScores,
    mkHighScores,
    getFirstPlace,
    getScores,
    addScore,
    Score (..),
    Points,
    mkScore,
  )
where

import Data.List (sort)
import GHC.Generics
import Data.Aeson
import Control.Applicative
import Control.Monad

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | A score's points
type Points = Int

-- | Score of a player
data Score = Score
  { name :: String,
    score :: Points
  }
  deriving (Generic, Show)

-- | The Highscores of a game / level
newtype HighScores = HighScores [Score] deriving (Eq, Generic, Show)

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------
-- instance Show Score where
--   show s = (show $ name s) ++ " : " ++ (show $ score s)

instance Eq Score where
  x == y = score x == score y

instance Ord Score where
  compare x y = compare (score x) (score y)

instance FromJSON Score where
--   parseJSON (Object v) =
--     Score <$> v .: "name"
--       <*> v .: "score"
--   parseJSON _ = mzero

instance ToJSON Score where
--   toJSON (Score name score) =
--     object
--       [ "name" .= name,
--         "score" .= score
--       ]

instance FromJSON HighScores where
instance ToJSON HighScores where
-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | Safe constructor for a new score
mkScore :: String -> Int -> Maybe Score
mkScore name score
  | score < 0 = Nothing
  | otherwise = Just Score {name = name, score = score}

mkHighScores :: [Score] -> HighScores
mkHighScores scores = HighScores scores

addScore :: Score -> HighScores -> HighScores
addScore score (HighScores scores) = mkHighScores (score : scores)

getFirstPlace :: HighScores -> Maybe Score
getFirstPlace (HighScores []) = Nothing
getFirstPlace (HighScores scores) = Just (maximum scores)

getScores :: HighScores -> [Score]
getScores (HighScores scores) = sort scores
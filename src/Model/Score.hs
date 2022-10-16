module Model.Score(
    HighScores, getFirstPlace,
    Score, Points, mkScore
) where

-------------------------------------------------------------------------------
-- Data structures
-------------------------------------------------------------------------------

-- | A score's points
type Points = Int

-- | Score of a player
data Score = Score{
    name  :: String,
    score :: Points
}

-- | The Highscores of a game / level
type HighScores = [Score]

-------------------------------------------------------------------------------
-- Type class implementations
-------------------------------------------------------------------------------

instance Eq Score where
    x == y = score x == score y

instance Ord Score where
    compare x y = compare (score x) (score y)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | Safe constructor for a new score
mkScore :: String -> Int -> Maybe Score
mkScore name score  | score < 0 = Nothing
                    | otherwise = Just Score { name = name, score = score }

getFirstPlace :: HighScores -> Maybe Score
getFirstPlace [] = Nothing
getFirstPlace highScores= Just (maximum highScores)

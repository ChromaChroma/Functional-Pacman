module Model.Score(
    HighScores, getFirstPlace,
    Score, Points, mkScore
) where

-- | A score's points
type Points = Int

-- | Score of a player
data Score = Score{
    name  :: String,
    score :: Points
}

instance Eq Score where
    x == y = score x == score y

instance Ord Score where
    compare x y = compare (score x) (score y)

-- | Safe constructor for a new score
mkScore :: String -> Int -> Maybe Score
mkScore name score  | score < 0 = Nothing
                    | otherwise = Just Score { name = name, score = score }

-- | The Highscores of a game / level
type HighScores = [Score]

getFirstPlace :: HighScores -> Maybe Score
getFirstPlace [] = Nothing
getFirstPlace highScores= Just (maximum highScores)

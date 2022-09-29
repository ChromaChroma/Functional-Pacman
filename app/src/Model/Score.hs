module Model.Score(Highscores, Score, mkScore) where

-- | The Highscores of a game / level
type HighScores = [Score]

getFirstPlace :: HighScores -> Maybe Score
getFirstPlace [] = Nothing
getFirstPlace = Just . head . sort

-- | Score of a player
data Score = Score{
    name  :: String
    score :: Int
    -- etc
}

-- | Eq implementation for Score
instance Eq Score where
    (==) = (==) `on` score

-- | Ord implementation for Score
instance Ord Score where
    compare = compare `on` score


-- | Safe constructor for a new score
mkScore :: String -> Int -> Maybe Score
mkScore name score  | score < 0 = Nothing
                    | otherwise = Just Score { name = name, score = score }
  


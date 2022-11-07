module Controller.ScoreController where

import Data.String
import System.IO.Error
import Control.Exception
import Data.Aeson
import Model.Score

-------------------------------------------------------------------------------
-- File locations
-------------------------------------------------------------------------------

highScoresFile :: FilePath
highScoresFile = "data/highscores.json"

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

writeJSON :: ToJSON a => a -> IO ()
writeJSON obj = do
  encodeFile highScoresFile obj
  

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

updateScores :: HighScores -> IO ()
updateScores hs = writeJSON hs

loadHighScores :: IO HighScores
loadHighScores = do
  json <- decodeFileStrict highScoresFile
  return $ case json of
    Just hs -> hs
    Nothing -> mkHighScores []
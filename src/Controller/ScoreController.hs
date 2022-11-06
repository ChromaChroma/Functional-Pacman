module Controller.ScoreController where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Model.Score

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

jsonFile :: FilePath
jsonFile = "data/highscores.json"

loadJSON :: IO B.ByteString
loadJSON = B.readFile jsonFile

writeJSON :: ToJSON a => a -> IO ()
writeJSON obj = encodeFile jsonFile obj

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

updateScores :: HighScores -> IO ()
updateScores hs = writeJSON hs

loadHighScores :: IO HighScores
loadHighScores = do
  d <- (eitherDecode <$> loadJSON)
  case d of
    Left err -> error err
    Right hs -> return $ hs
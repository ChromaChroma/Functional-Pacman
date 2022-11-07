module Controller.ScoreController where

import Data.String
import System.IO.Error
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Model.Score
import System.Directory (createDirectoryIfMissing, doesFileExist)

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
  
loadJSON :: FilePath -> IO B.ByteString
loadJSON jsonFilePath= do
  res <- safeRead jsonFilePath
  case res of
    Just x -> return $ fromString x
    Nothing -> return $ B.empty

safeRead :: FilePath -> IO (Maybe String)
safeRead path = (fmap Just $ readFile path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe String)
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

updateScores :: HighScores -> IO ()
updateScores hs = writeJSON hs

loadHighScores :: IO HighScores
loadHighScores = do
  d <- (eitherDecode <$> loadJSON highScoresFile)
  return $ case d of
    Right hs -> hs
    Left err -> mkHighScores []
module Controller.LevelController(
  serializeLevel,
  serializeLevels,
  deserializeLevel,
  deserializeLevels,
  ) where

-- | Serialize a Level into a string to be stored as text
serializeLevel :: Level -> String
serializeLevel level = undefined

-- | Serialize list of Levels into a string to be stored as text
serializeLevels :: [Level] -> String
serializeLevels = map serializeLevel

-- | Deserialize a string into a Level
deserializeLevel :: String -> Level
deserializeLevel str = undefined

-- | Deserialize a string into a list of Levels
deserializeLevels :: String -> [Level]
deserializeLevels = map deserializeLevel
module View.Terminal(printGame) where 

import Model.Characters
import Model.Game hiding (player)
import Model.Items
import Model.Level

-- | Implementations of show for all data objects that shold be represented in the string that will be printed in the terminal
instance Show PointItem where
  show Dot {} = "·"
  show PowerPellet {} = "*"
  show Fruit {} = "F"
  
instance Show Tile where
  show Wall = "■"
  show Floor = " "
  show (GhostDoor Open) = "O"
  show (GhostDoor Closed) = "C"

instance Show Player where
  show Player {} = "P"

instance Show Ghost where
  show Ghost {} = "G"

-- | Functions to build a string representation of the level including the layout, player, enemies and items
printGame :: GameState -> IO ()
printGame (GameState status player level elapsedTime) = do
  putStrLn $ "Elapsed time: " ++ show elapsedTime
  putStrLn $ "Lives left: " ++ show (amount (pLives player))
  printLevel level 

-- | Prints level info and the level layout
printLevel :: Level -> IO ()
printLevel level = do
  putStrLn ("Level: " ++ show (levelNumber level))
  putStrLn ("Active Ghosts: " ++ showGhosts (enemies level) )
  mapM_ (putStrLn . concatMap (\x -> " " ++ x ++ " ")) (toStringMatrix level)

showGhosts :: [Ghost] -> String
showGhosts [] = ""
showGhosts ((g):gs) = show (gName g) ++ " " ++ showGhosts gs

-- | Functions to build a string representation of the level including the layout, player, enemies and items
toStringMatrix :: Level -> [[String]]
toStringMatrix level = projectPlayer (player level)
  $ projectGhosts (enemies level) 
  $ projectItems (items level) 
  $ layoutToStringArray (layout level)

layoutToStringArray :: [[Tile]] -> [[String]]
layoutToStringArray = map (map show)

projectItems :: [PointItem] -> [[String]] -> [[String]]
projectItems items layoutArray = foldl (\layout item -> updateMatrix layout (show item) (intPosition(position item))) layoutArray items
      
projectGhosts :: [Ghost] -> [[String]] -> [[String]]
projectGhosts ghosts layoutArray = foldl (\layout ghost -> updateMatrix layout (show ghost) (intPosition(gPosition ghost))) layoutArray ghosts

projectPlayer :: Player -> [[String]]  -> [[String]]
projectPlayer player layout = updateMatrix layout (show player) (intPosition(pPosition player))

intPosition :: Position -> (Int, Int)
intPosition (x, y) = (round x, round y)

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
  

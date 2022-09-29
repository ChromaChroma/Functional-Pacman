module Model.Level where 

-- | Number/id of the level
type LevelNumber = Int

-- | Different types of tiles a level can have
-- | Wall is a tile player nor ghost can move through
-- | Floor is a tile player and ghost can move through 
-- | Door is a tile ghost can move through, but player can't, given that the doors are open
data Tile = Wall | Floor | GhostDoor Doorstate deriving (Eq, Show)

-- | State of the ghost door
data DoorState = Open | Closed deriving (Eq, Show)

-- | Level layout as a 2D Tile matrix
type LevelLayout = [[Tile]]

-- | Position in the level
type Position = (Int, Int)

-- | Type of pallet
-- | Normal pellet is a pellet that gives points
-- | Power pellet is a pellet that makes ghosts vulnerable
data PelletType = Normal | Power deriving (Eq, Show)
-- Optional extention of power pellet types
-- data PalletType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key deriving (Eq, Show)

-- | Pellet in the level
data Pellet = Pellet {
    position  :: Position
    type      :: PelletType
}

-- | Level data
data Level = Level {
    id      :: LevelNumber
    name    :: String
    layout  :: LevelLayout
    pellets :: [Pellet]
    enemies :: [Enemy]
}
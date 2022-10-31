module Model.Dijkstra where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Maybe (catMaybes, fromMaybe)

import Model.Level
import Model.Utils

{-

This module implements Dijkstra's algorithm for finding the shortest path

-}

data Distance a = Dist a | Infinity deriving (Eq, Show)

instance Ord a => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

newtype Graph = Graph {edges :: HashMap Intersection [(Intersection, Int)]} deriving (Show)

type Intersection = (Int, Int)

data DijkstraState = DijkstraState
  { visitedSet :: HashSet Intersection,
    distanceMap :: HashMap Intersection (Distance Int),
    nodeQueue :: MinPrioHeap (Distance Int) Intersection
  }

findShortestDistance :: Graph -> Intersection -> Intersection -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> HashMap Intersection (Distance Int)
    processQueue ds@(DijkstraState visited distMap nQueue) = case H.view nQueue of
      Nothing -> distMap
      Just ((minDist, node), queue') ->
        if node == dest then distMap
        else 
            if HS.member node visited then processQueue (ds {nodeQueue = queue'})
            else -- Update the visited set
                let visited' = HS.insert node visited -- Get all unvisited neighbors of our current node
                    allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
                    unvisitedNeighbors = filter (\(n, _) -> not (HS.member n visited')) allNeighbors
                 in -- Fold each neighbor and recursively process the queue
                    processQueue $ foldl (foldNeighbor node) (DijkstraState visited' distMap queue') unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState visited' distMap queue') (neighborNode, cost) =
      let altDistance = addDist (distMap !?? current) (Dist cost)
       in if altDistance < distMap !?? neighborNode
            then DijkstraState visited' (HM.insert neighborNode altDistance distMap) (H.insert (altDistance, neighborNode) queue')
            else ds


graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ((1,1), [((1,4), 100), ((1,2), 1), ((1,3), 20)])
  , ((1,2), [((1,4), 50)])
  , ((1,3), [((1,4), 20)])
  , ((1,4), [])
  ]




 

findShortestDistanceOrLevel :: Level -> Intersection -> Intersection -> Distance Int
findShortestDistanceOrLevel lvl pos pos2 = findShortestDistance (Graph $ levelToEdgeMap lvl pos pos2) pos pos2

levelToEdgeMap :: Level -> Intersection -> Intersection ->  HashMap Intersection [(Intersection, Int)]
levelToEdgeMap lvl p p2 = pointsToPointEdgesMap
  where
    (w, h) = layoutSize $ layout lvl
    splitPoints = p : p2 : levelFloorSplits lvl 
  
    
    -- Convert splitpoints to duos
    -- Or update splitpoints so that they are duos of points that are reachable to eachother
    edgeMap :: HashMap Intersection [(Intersection, Int)]
    edgeMap = HM.empty

    -- splitPointsWithEdges = map (\pos -> pos (x, y, getEdges lvl (x, y))) splitPoints
    pointsToPointEdgesMap = foldr (\pos acc -> addPointEdges pos acc) edgeMap splitPoints

    addPointEdges :: Intersection -> HashMap Intersection [(Intersection, Int)] -> HashMap Intersection [(Intersection, Int)]
    addPointEdges pos edgeMap = HM.insert pos (getEdges lvl pos) edgeMap

    getEdges :: Level -> Intersection -> [(Intersection, Int)]
    getEdges lvl pos = map (\p -> (p, calcDist pos p)) (nextNeigbors)
      where
        r :: Intersection -> Maybe Intersection
        r (x, y) = trans (x, y) (\(x, y) -> ((x + 1) `intmod'` w, y)) (x, y)
        
        l :: Intersection -> Maybe Intersection
        l (x, y) = trans (x, y) (\(x, y) -> ((x - 1) `intmod'` w, y)) (x, y)
        
        u :: Intersection -> Maybe Intersection
        u (x, y) = trans (x, y) (\(x, y) -> (x, (y + 1) `intmod'` h)) (x, y)
        
        d :: Intersection -> Maybe Intersection
        d (x, y) = trans (x, y) (\(x, y) -> (x, (y - 1) `intmod'` h)) (x, y)

        nextNeigbors = catMaybes [r pos, l pos, u pos, d pos]
        -- map (\(x, y)  -> (x + posx, y + posy)) [(0, 1), (0, -1), (1, 0), (-1, 0)]

        -- | Takes the original position, transformer and node that will be transformed
        trans :: Intersection -> (Intersection -> Intersection) -> Intersection -> Maybe Intersection
        trans originalPos f pos
          | originalPos == nPos = Nothing 
          | tileAtW lvl nPos /= Floor = Nothing
          | nPos `elem` splitPoints = Just nPos
          | otherwise = trans originalPos f nPos
          where 
            nPos = f pos

    calcDist :: Intersection -> Intersection -> Int
    calcDist (x1, y1) (x2, y2)
      | x1 == x2 = abs (y1 - y2)
      | y1 == y2 = abs (x1 - x2)

module Model.Dijkstra where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Maybe (mapMaybe, fromMaybe)
import Model.Level
import Model.Utils

-------------------------------------------------------------------------------
-- Types and structures for the Dijkstra's algorithm
-------------------------------------------------------------------------------

type Node = (Int, Int)

type Edge = (Node, Int)

type Graph = HashMap Node [Edge]

data DijkstraState = DijkstraState
  { visitedSet :: HashSet Node,
    distanceMap :: HashMap Node (Distance Int),
    nodeQueue :: MinPrioHeap (Distance Int) Node
  }

data Distance a = Dist a | Infinity deriving (Eq, Show)

instance Ord a => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

-------------------------------------------------------------------------------
-- Algorithm helper functions
-------------------------------------------------------------------------------

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

-------------------------------------------------------------------------------
-- Main algorithm
-------------------------------------------------------------------------------

findShortestDistance :: Graph -> Node -> Node -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> HashMap Node (Distance Int)
    processQueue ds@(DijkstraState visited distMap nQueue) = case H.view nQueue of
      Nothing -> distMap
      Just ((_, node), queue') ->
        let processNode
              | node == dest = distMap
              | HS.member node visited = processQueue (ds {nodeQueue = queue'})
              | otherwise = processQueue $ foldl (foldNeighbor node) (DijkstraState visited' distMap queue') unvisitedNeighbors
              where
                visited' = HS.insert node visited
                allNeighbors = fromMaybe [] (HM.lookup node graph)
                unvisitedNeighbors = filter (\(n, _) -> not (HS.member n visited')) allNeighbors
         in processNode
         
    foldNeighbor current ds@(DijkstraState visited' distMap queue') (neighborNode, cost)
      | altDistance < distMap !?? neighborNode = DijkstraState visited' (HM.insert neighborNode altDistance distMap) (H.insert (altDistance, neighborNode) queue')
      | otherwise = ds
      where 
        altDistance = addDist (distMap !?? current) (Dist cost)

-------------------------------------------------------------------------------
-- Functions for applying Dijkstra's algorithm to a lLevel
-------------------------------------------------------------------------------

-- | Calculates if there is any valid path from node n to n'
isReachable :: Level -> Node -> Node -> Bool
isReachable lvl n n' = shortestPath lvl n n' /= Infinity

-- | Find the shortest path between 2 points in a Level
shortestPath :: Level -> Node -> Node -> Distance Int
shortestPath lvl n n'
  | isOnValidTile n && isOnValidTile n' = findShortestDistance (levelToGraph lvl n n') n n'
  | otherwise = Infinity
  where
    isOnValidTile n = tileAtW lvl n == Floor

-- | Convert a Level to a Graph
levelToGraph :: Level -> Node -> Node -> Graph
levelToGraph lvl n n'  = foldr addPointEdges HM.empty nodes
  where
    nodes = n : n'  : levelFloorSplits lvl
    addPointEdges node edges = HM.insert node (getEdges lvl node) edges

    getEdges :: Level -> Node -> [Edge]
    getEdges lvl node = map (\n -> (n, dist node n)) (nextNeigbors)
      where
        nextNeigbors = mapMaybe (\translation -> translateNode (translation `add`) node) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        add (x, y) (x', y') = (x + x', y + y')

        translateNode :: (Node -> Node) -> Node -> Maybe Node
        translateNode f node
          | node == node' = Nothing
          | tileAtW lvl node' /= Floor = Nothing
          | node' `elem` nodes = Just node'
          | otherwise = translateNode f node'
          where
            node' = f node

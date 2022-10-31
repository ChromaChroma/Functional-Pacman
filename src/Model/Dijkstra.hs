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

This module implements Dijkstra's algorithm for finding the shortest path between 2 points

-}

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

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

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
-- Level specific functions for finding the shortest path in a Pac-Man level
-------------------------------------------------------------------------------

-- | Find the shortest path between 2 points in a level
shortestPath :: Level -> Node -> Node -> Distance Int
shortestPath lvl pos pos2
  | isValidPosition pos && isValidPosition pos = findShortestDistance (levelToGraph lvl pos pos2) pos pos2
  | otherwise = Infinity
  where
    isValidPosition p = tileAtW lvl p == Floor

-- | Convert a level to a graph
levelToGraph :: Level -> Node -> Node -> Graph
levelToGraph lvl p p2 = foldr addPointEdges HM.empty nodes
  where
    nodes = p : p2 : levelFloorSplits lvl
    addPointEdges pos edges = HM.insert pos (getEdges lvl pos) edges

    getEdges :: Level -> Node -> [Edge]
    getEdges lvl pos = map (\p -> (p, dist pos p)) (nextNeigbors)
      where
        nextNeigbors = catMaybes $ map (\translation -> translateNode (\p -> p `add` translation) pos) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        add (x, y) (x', y') = (x + x', y + y')

        translateNode :: (Node -> Node) -> Node -> Maybe Node
        translateNode f node
          | pos == nPos = Nothing
          | tileAtW lvl nPos /= Floor = Nothing
          | nPos `elem` nodes = Just nPos
          | otherwise = translateNode f nPos
          where
            nPos = f node

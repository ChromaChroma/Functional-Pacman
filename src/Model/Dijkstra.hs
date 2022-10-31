module Model.Dijkstra where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Model.Level
import Model.Utils

{-

This module implements Dijkstra's algorithm for finding the shortest path

-}

type Node = (Int, Int)

type Edge = (Node, Int)

newtype Graph = Graph {edges :: HashMap Node [Edge]} deriving (Show)

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
      Just ((_minDist, node), queue') ->
        let processNode
              | node == dest = distMap
              | HS.member node visited = processQueue (ds {nodeQueue = queue'})
              | otherwise = processQueue $ foldl (foldNeighbor node) (DijkstraState visited' distMap queue') unvisitedNeighbors
              where
                visited' = HS.insert node visited -- Get all unvisited neighbors of our current node
                allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
                unvisitedNeighbors = filter (\(n, _) -> not (HS.member n visited')) allNeighbors
         in processNode

    foldNeighbor current ds@(DijkstraState visited' distMap queue') (neighborNode, cost) =
      let altDistance = addDist (distMap !?? current) (Dist cost)
       in if altDistance < distMap !?? neighborNode
            then DijkstraState visited' (HM.insert neighborNode altDistance distMap) (H.insert (altDistance, neighborNode) queue')
            else ds

shortestPath :: Level -> Node -> Node -> Distance Int
shortestPath lvl pos pos2
  | isValidPosition pos && isValidPosition pos = findShortestDistance (Graph $ levelToEdgeMap lvl pos pos2) pos pos2
  | otherwise = Infinity
  where
    isValidPosition p = tileAtW lvl p == Floor

levelToEdgeMap :: Level -> Node -> Node -> HashMap Node [Edge]
levelToEdgeMap lvl p p2 = foldr addPointEdges HM.empty nodes
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

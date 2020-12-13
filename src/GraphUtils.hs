module GraphUtils where

import Blueprint
import Data.List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Geometry
import Machine
import Wire

-- | A Graph is a map from a node to its outNeighbors
newtype Graph = Graph (Map.Map Int (Set Int)) deriving (Show, Eq)

pointToInt :: Blueprint -> Point -> Int
pointToInt Blueprint {width = w} Point {pointX = x, pointY = y} = x + y * w

intToPoint :: Blueprint -> Int -> Point
intToPoint Blueprint {width = w} n = Point (n `safeMod` w) (n `div` w)
  where
    safeMod m d = if d == 0 then 0 else m `mod` d

blueprintToGraph :: Blueprint -> Graph
blueprintToGraph b@Blueprint {grid = g} = Graph $ Map.fromList $ Prelude.map toEdge (allPoints b)
  where
    toEdge p = (pointToInt b p, Set.fromList $ Prelude.map (pointToInt b) (openAdjacentPoints p))
    adjacentPoints p = Prelude.filter (`isInBounds` b) [p +>> Point 0 1, p +>> Point 1 0, p ->> Point 0 1, p ->> Point 1 0]
    isOpen p p' = case Map.lookup p' g of
      Nothing -> True
      Just (Wire t) -> case p ->> p' of
        Point x 0 | x /= 0 -> not (connectsToEast t || connectsToWest t)
        Point 0 y | y /= 0 -> not (connectsToNorth t || connectsToSouth t)
        _ -> error "Non-adjacent point passed to isOpen"
      _ -> False
    openAdjacentPoints p = Prelude.filter (isOpen p) (adjacentPoints p)

-- | Run a bfs on a tree, producing a map from children to parents
--     graph     queue   parents        updated parents
bfs :: Graph -> [Int] -> Map Int Int -> Map Int Int
bfs _ [] parents = parents
bfs g xs parents = bfs g xs' parents'
  where
    xs' = Prelude.filter (`Map.notMember` parents) $ children g xs
    parents' = Prelude.foldr addChildrenToParents parents xs
    addChildrenToParents x m = Prelude.foldr (`insertIfNew` x) m (children g [x])

bfsFrom :: Graph -> Int -> Map Int Int
bfsFrom g x = bfs g [x] Map.empty

-- | Given a graph and a list of nodes,
-- | return the list of all children of nodes in the list.
children :: Graph -> [Int] -> [Int]
children (Graph m) ys = Set.toList $ Set.unions (Maybe.mapMaybe (`Map.lookup` m) ys)

-- | Utility to insert a pair into a map iff the key doesn't exist
insertIfNew :: Ord k => k -> a -> Map k a -> Map k a
insertIfNew x y m = if x `Map.member` m then m else Map.insert x y m
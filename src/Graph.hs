module Graph where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)

-- | undirected graphs with at most one edge between vertices

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = Map Vertex IntSet

-- | the empty graph
emptyGraph :: Graph
emptyGraph = Map.empty

-- | every edge in the graph in unspecified order
edges :: Graph -> [Edge]
edges = concat . Map.mapWithKey (\k -> fmap (k,) . filter (>k) . IntSet.toList)

-- | add a single vertex to the graph
addVertex :: Vertex -> Graph -> Graph
addVertex = Map.alter insert
  where
    insert Nothing = Just IntSet.empty
    insert (Just vs) = Just vs

-- | add mutiple vertices to the graph
addVertices :: [Vertex] -> Graph -> Graph
addVertices vs g = foldl' (flip addVertex) g vs

-- | remove a vertex from the graph
deleteVertex :: Vertex -> Graph -> Graph
deleteVertex v = Map.map (IntSet.delete v) . Map.delete v

-- | remove multiple vertices from the graph
deleteVertices :: [Vertex] -> Graph -> Graph
deleteVertices vs g = foldl' (flip deleteVertex) g vs

-- | add an edge to the graph
addEdge :: Edge -> Graph -> Graph
addEdge (a, b) = connect b a . connect a b . addVertices [a,b]
  where
    connect x y = Map.adjust (IntSet.insert y) x

-- | add multiple edges to the graph
addEdges :: [Edge] -> Graph -> Graph
addEdges es g = foldl' (flip addEdge) g es

-- | remove an edge from the graph
deleteEdge :: Edge -> Graph -> Graph
deleteEdge (a, b) = disconnect b a . disconnect a b
  where
    disconnect x y = Map.alter (fmap $ IntSet.delete y) x

-- | remove multiple edges from the graph
deleteEdges :: [Edge] -> Graph -> Graph
deleteEdges es g = foldl' (flip deleteEdge) g es

-- | all vertices which are connected by an edge to the given vertex
neighbors :: Vertex -> Graph -> [Vertex]
neighbors v = fromMaybe [] . fmap IntSet.toList . Map.lookup v

-- | generate a planar graph representing a grid of adjacent squares
squareGrid :: Int -> Int -> Graph
squareGrid width height = addEdges es $ addVertices vs emptyGraph
  where
    vs = [0..n-1]
    n = width * height
    es = concatMap out vs
    out v = fmap (v,) (adj v)
    adj v = catMaybes $ fmap ($ v) [up, down, left, right]
    up v = let x = v - width in if x >= 0 then Just x else Nothing
    down v = let x = v + width in if x < n then Just x else Nothing
    left v = if v `mod` width > 0 then Just (v - 1) else Nothing
    right v = if v`mod` width < width - 1 then Just (v + 1) else Nothing

-- | generate a planar graph representing a grid of adjacent hexagons
hexGrid :: Int -> Int -> Graph
hexGrid width height = addEdges es $ addVertices vs emptyGraph
  where
    vs = [0..n-1]
    n = width * height 
    es = concatMap out vs
    out v = fmap (v,) (adj v)
    adj v = catMaybes $ fmap ($ v) [upLeft, up, upRight, downLeft, down, downRight]

    firstCol = 0
    lastCol = width - 1
    firstRow = 0
    lastRow = height - 1

    col v = v `mod` width
    row v = v `div` width

    isEven x = x `mod` 2 == 0

    up' v = v - width
    down' v = v + width

    upLeft v = if row v == firstRow || col v == firstCol then Nothing
               else if isEven (col v) then Just $ (up' v) - 1 else Just (v - 1)
    up v = if row v == firstRow then Nothing else Just $ up' v
    upRight v = if row v == firstRow || col v == lastCol then Nothing
                else if isEven (col v) then Just $ up' v + 1 else Just (v + 1)
    downLeft v = if row v == lastRow || col v == firstCol then Nothing
                 else if isEven (col v) then Just (v - 1) else Just $ down' v - 1
    down v = if row v == lastRow then Nothing else Just $ down' v
    downRight v = if row v == lastRow || col v == lastCol then Nothing
                  else if isEven (col v) then Just (v + 1) else Just $ down' v + 1


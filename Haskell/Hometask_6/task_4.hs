--Для типа Graph, разбиравшегося на паре, реализовать алгоритм Дейкстры поиска кратчайшего пути. 
--Поиск пути должен производиться с использованием экземпляров класса типов Monad или MonadPlus

import Data.Graph
import qualified Data.Map as Map

type Weight = Int
type EdgeExt = (Vertex, Vertex, Weight)

getEdge :: EdgeExt -> Edge
getEdge (a, b, w) = (a, b)

getWeight :: EdgeExt -> Weight
getWeight (a, b, w) = w

data GraphExt = GraphExt
                { 
                    plainGraph :: Graph
                    , edgeWeights :: Map.Map Edge Weight
                }
                deriving (Show, Eq)

inf = 1 / 0

buildGraphExt :: Bounds -> [EdgeExt] -> GraphExt
buildGraphExt vertexBounds edgeExts =   GraphExt 
                                        (buildG vertexBounds (map (\eE -> getEdge eE) edgeExts))
                                        (Map.fromList (map (\eExt -> (getEdge eExt, getWeight eExt)) edgeExts))


{-
--dijkstra :: Vertex -> Graph -> [Int]
dijkstra graph s = dijkstraRecc graph s (take (length (vertices graph)) (repeat INF)) (take (length (vertices graph)) (repeat False))
    where
    dijkstraRecc graph s distancies visited = relaxFrom (edgesFromV minVertex) distancies
        where
        minVertex =
        edgesFromV v = filter (\(from, to) -> from == v) (edges graph)
        relaxFrom edges distancies = 
 -}






graph = buildGraphExt (1, 6) [(1, 2, 10), (1, 3, 20), (2, 4, 30), (5, 6, 40)]


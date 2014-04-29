import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

type Weight = Double
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

-- list aux functions
remove :: Eq a => a -> [a] -> [a]
remove x = foldr (\k acc -> if (x == k) then acc else k : acc) []

update :: Int -> a -> [a] -> [a]
update index value xs = take (index - 1) xs ++ value : (drop index xs)

minimumSnd :: [(Vertex, Weight)] -> (Vertex, Weight)
minimumSnd = foldr (\x@(f,s) acc@(fa, sa)-> if (s <= sa) then x else acc) (0, inf)
---------------------


dijkstra :: Vertex -> GraphExt -> [Weight]
dijkstra s graphExt = dijkstraRecc graphExt startWeights toVisit
    where
    startWeights = map (\v -> if (v == s) then 0 else inf) toVisit
    toVisit = vertices $ plainGraph graphExt

    dijkstraRecc :: GraphExt -> [Weight] -> [Vertex] -> [Weight]
    dijkstraRecc _ ds [] = ds
    dijkstraRecc graphExt distances toVisit = dijkstraRecc graphExt relaxedDist (remove minVertex toVisit)
        where
        minVertex = fst $ minimumSnd (filter (\(id, len) -> id `elem` toVisit) indexedDistances)
        relaxedDist = map (\(inx, dist) -> if (inx `elem` edgesEnd) then relax inx dist else dist) indexedDistances        
        edgesFromV = filter (\(from, to) -> from == minVertex) (edges (plainGraph graphExt))
        edgesEnd = snd $ unzip edgesFromV
        indexedDistances = zip [1..] distances

        relax :: Vertex -> Weight -> Weight
        relax vTo oldDist = min oldDist (distances!!(minVertex - 1) + (Data.Maybe.fromJust (Map.lookup (minVertex, vTo) (edgeWeights graphExt))))



graph = buildGraphExt (1, 6) [(1, 2, 10), (1, 3, 20), (2, 4, 30), (5, 6, 40)]  --[0, 10, 20, 40, Infinity, Infinity] from 1


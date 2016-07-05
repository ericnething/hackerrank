module RouteBetweenNodes where

import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.List (foldl', reverse)

type Vertex = Int
type Graph = Vector [Vertex]

dfs :: Graph -> Vertex -> [Vertex]
dfs graph = reverse . snd . go Set.empty []
  where
    -- Mark this node as "visited" and process each node in the adjacency list
    go visited result node =
      foldl' dfs' (Set.insert node visited, node : result) (graph ! node)

    -- If this node has not been visited, recurse on its adjacency list
    dfs' (visited, result) node
      | Set.notMember node visited = go visited result node
      | otherwise = (visited, result)

routeFromTo :: Vertex -> Vertex -> Graph -> Bool
routeFromTo n1 n2 graph = n2 `elem` dfs graph n1

-- | For this sample graph:
-- Depth-First traversal from 0 = [0,1,5,4,6,2,7,3]
-- Breadth-First traversal from 0 = [0,1,2,5,7,4,6,3]
g1 :: Graph
g1 = V.fromList [ [1,2,5] -- Vertex 0
                , [5]     -- Vertex 1
                , [7]     -- Vertex 2
                , []      -- Vertex 3
                , []      -- Vertex 4
                , [4,6]   -- Vertex 5
                , [2,7]   -- Vertex 6
                , [3]     -- Vertex 7
                ]

main :: IO ()
main = do
  putStrLn $ show g1
  putStrLn $ "Is there a route from node 0 to node 7?"
  putStrLn $ show (routeFromTo 0 7 g1)

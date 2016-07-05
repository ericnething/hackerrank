{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Sequence as SQ
import           Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import           Data.List

type Vertex = Int
type Graph = Vector [Vertex]

-- | Depth-First Search
dfs :: Graph -> Vertex -> [Vertex]
dfs graph node = reverse . snd $ go Set.empty [] node
  where
    go visited result node =
      foldl' dfs' (Set.insert node visited, node : result) (graph ! node)

    dfs' (visited, result) node
      | Set.notMember node visited = go visited result node
      | otherwise = (visited, result)

-- | Breadth-First Search
bfs :: Graph -> Vertex -> [Vertex]
bfs graph node = reverse . snd $
                 -- Seed the queue with the first node and add it to
                 -- the visited Set
                 go (Set.singleton node) (SQ.singleton node) []
  where
    -- If the queue is empty, we are done.
    go visited (SQ.viewl -> SQ.EmptyL) result = (visited, result)

    -- Otherwise, dequeue the next node and process its adjacency
    -- list.
    go visited (SQ.viewl -> node :< queue) result =
      let (v,q,r) = foldl' bfs' (visited, queue, node : result) (graph ! node)
      in go v q r

    bfs' (visited, queue, result) node
      -- If this node has not been visited, add it to the queue and
      -- mark it as visited.
      | Set.notMember node visited = (Set.insert node visited, queue |> node, result)

      -- If it has been visited, continue.
      | otherwise = (visited, queue, result)

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
  putStrLn $ "dfs: " ++ show (dfs g1 0)
  putStrLn $ "bfs: " ++ show (bfs g1 0)
  

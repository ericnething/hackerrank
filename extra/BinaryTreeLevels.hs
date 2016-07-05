{-# LANGUAGE ViewPatterns #-}

module BinaryTreeLevels where

import qualified Data.Sequence as SQ
import           Data.Sequence (Seq, (|>), ViewL(..))

data Bin a = Node a (Bin a) (Bin a) | Empty deriving Show

singleton :: a -> Bin a
singleton a = Node a Empty Empty

empty :: Bin a
empty = Empty

levelOrder :: Bin a -> [a]
levelOrder t = reverse $ go (SQ.singleton t) []
  where
    -- If the queue is empty, we are done.
    go (SQ.viewl -> SQ.EmptyL) result = result
    
    -- Otherwise, continue.
    go (SQ.viewl -> x :< queue) result = case x of
      
      -- If the node is empty, ignore it and keep going.
      Empty      -> go queue result

      -- Append the node's children to the queue and combine this
      -- node's value with the result.
      Node a l r -> go (queue |> l |> r) (a : result)

testTree2 :: Bin Int
testTree2 = Node 6
            (Node 4 (singleton 3) Empty)
            (Node 8 (singleton 7) (Node 9 Empty (Node 10 Empty Empty)))

main :: IO ()
main = do
  putStrLn $ show $ levelOrder testTree2
  

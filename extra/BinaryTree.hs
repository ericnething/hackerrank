{-# LANGUAGE ViewPatterns #-}

module BinaryTree where

import qualified Data.Sequence as SQ
import           Data.Sequence (Seq, (|>), ViewL(..))
import           Data.List
import           Data.Monoid ((<>))

-- | Binary tree datatype
data Bin a = Node a (Bin a) (Bin a) | Empty
           deriving Show

instance Functor Bin where
  fmap _ Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

-- | An empty binary tree
empty :: Bin a
empty = Empty

-- | A single node
singleton :: a -> Bin a
singleton a = Node a Empty Empty

-- | Check if the binary tree is empty
null :: Bin a -> Bool
null Empty = True
null _     = False

-- | Pre-order traversal
preOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
preOrder Empty = mempty
preOrder (Node a l r) = pure a <> preOrder l <> preOrder r

-- | In-order traversal
inOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
inOrder Empty = mempty
inOrder (Node a l r) = inOrder l <> pure a <> inOrder r

-- | Post-order traversal
postOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
postOrder Empty = mempty
postOrder (Node a l r) = postOrder l <> postOrder r <> pure a

-- | Level-order traversal
levelOrder :: Bin a -> [a]
levelOrder t = reverse $ go (SQ.singleton t) []
  where
    -- If the queue is empty, we are done.
    go (SQ.viewl -> SQ.EmptyL) result = result

    -- Otherwise, continue.
    go (SQ.viewl -> x :< queue) result = case x of

      -- If the node is empty, continue.
      Empty -> go queue result

      -- If the node is not empty, add it's value to the result, push
      -- its children onto the queue, and continue.
      Node a l r -> go (queue |> l |> r) (a : result)


testTree2 :: Bin Int
testTree2 = Node 6
            (Node 4 (singleton 3) Empty)
            (Node 8 (singleton 7) (Node 9 Empty (Node 10 Empty Empty)))

main :: IO ()
main = do
  putStrLn ""

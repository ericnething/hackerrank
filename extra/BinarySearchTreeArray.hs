module BinarySearchTreeArray where

import qualified Data.Vector as V
import           Data.Vector (Vector, (!))

data BST a = Node a (BST a) (BST a) | Empty deriving (Show, Eq)

singleton :: Ord a => a -> BST a
singleton a = Node a Empty Empty

empty :: BST a
empty = Empty

insert :: Ord a => a -> BST a -> BST a
insert a Empty = singleton a
insert a (Node x l r) | a < x = Node x (insert a l) r
                      | a > x = Node x l (insert a r)
                      | otherwise = Node x l r

arrayToBST :: Ord a => Vector a -> BST a
arrayToBST v = go (0, length v - 1)
  where go (low, high)
          | low > high  = Empty
          | otherwise = Node (v ! idx) (go (low, idx - 1)) (go (idx + 1, high))
          where idx = (high - low) `div` 2 + low

main :: IO ()
main = do
  let v1 = V.fromList [1..10]
      v2 = V.fromList [1..5]
  putStrLn $ show v1
  putStrLn $ show $ arrayToBST v1
  
  putStrLn $ show v2
  putStrLn $ show $ arrayToBST v2

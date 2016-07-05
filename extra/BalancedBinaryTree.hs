module BalancedBinaryTree where

import qualified Data.List as L
import Data.Monoid ((<>))

data Bin a = Node a (Bin a) (Bin a)
           | Empty
           deriving (Eq, Show)

instance Functor Bin where
  fmap f Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Foldable Bin where
  foldMap f Empty = mempty
  foldMap f (Node a l r) = foldMap f l <> f a <> foldMap f r

preOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
preOrder Empty = mempty
preOrder (Node a l r) = pure a <> preOrder l <> preOrder r

inOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
inOrder Empty = mempty
inOrder (Node a l r) = inOrder l <> pure a <> inOrder r

postOrder :: (Foldable t, Applicative t, Monoid (t a)) => Bin a -> t a
postOrder Empty = mempty
postOrder (Node a l r) = postOrder l <> postOrder r <> pure a

testTree1 :: Bin Int
testTree1 = Node 6
            (Node 4 (singleton 3) Empty)
            (Node 8 (singleton 7) (Node 9 Empty (Node 10 Empty (singleton 12))))

testTree2 :: Bin Int
testTree2 = Node 6
            (Node 4 (singleton 3) Empty)
            (Node 8 (singleton 7) (Node 9 Empty (Node 10 Empty Empty)))
            
insert :: (Ord a, Eq a) => a -> Bin a -> Bin a
insert x Empty = singleton x
insert x (Node a l r) | x < a     = Node a (insert x l) r
                      | otherwise = Node a l (insert x r)

singleton :: a -> Bin a
singleton x = Node x Empty Empty

empty :: Bin a
empty = Empty

depth :: Bin a -> Int
depth Empty = (-1)
depth (Node a l r) = 1 + max (depth l) (depth r)

isBalanced :: Bin a -> Bool
isBalanced = go
  where go Empty = True
        go (Node a l r) | abs ((depth l) - (depth r)) > 1 = False
                        | otherwise = True && go l && go r

-- combine the check for balanced subtrees and the calculation of the
-- depth
isBalanced2 :: Bin a -> Bool
isBalanced2 = fst . go
  where go Empty = (True, -1)
        go (Node a l r) = f (go l) (go r)
        f (a1, b1) (a2, b2) = (a1 && a2 && abs (b1 - b2) <= 1, 1 + max b1 b2)

main :: IO ()
main = do
  putStrLn $ show testTree1 ++ " is balanced?"
  putStrLn $ show (isBalanced2 testTree1)

  putStrLn $ show testTree2 ++ " is balanced?"
  putStrLn $ show (isBalanced2 testTree2)

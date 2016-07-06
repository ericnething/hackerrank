module BinarySubTree where

import Prelude hiding (null)
import Data.Monoid ((<>))

data Bin a = Empty | Node a (Bin a) (Bin a)
           deriving (Show, Eq)

instance Functor Bin where
  fmap f Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Foldable Bin where
  foldMap f Empty = mempty
  foldMap f (Node a l r) = foldMap f l <> f a <> foldMap f r

empty :: Bin a
empty = Empty

null :: Bin a -> Bool
null Empty = True
null _     = False

singleton :: a -> Bin a
singleton a = Node a Empty Empty

isSubTreeOf :: Eq a => Bin a -> Bin a -> Bool
isSubTreeOf t1 t2 = or $ go t2
  where
    go Empty           = []
    go t2@(Node a l r) = [t1 `isEqual` t2] ++ go l ++ go r

isEqual :: Eq a => Bin a -> Bin a -> Bool
isEqual = go
  where go Empty Empty = True
        go _     Empty = False
        go Empty _     = False
        go t1@(Node a1 l1 r1) t2@(Node a2 l2 r2)
          | a1 == a2  = go l1 l2 && go r1 r2
          | otherwise = False

testTree1 :: Bin Int
testTree1 = Node 5
            (Node 3 (singleton 2) (singleton 4))
            (Node 7 (singleton 6) (Node 8 Empty (singleton 9)))

testTree2 :: Bin Int
testTree2 = Node 7 (singleton 6) (Node 8 Empty (singleton 9))


main :: IO ()
main = do
  print $ testTree2 `isSubTreeOf` testTree1

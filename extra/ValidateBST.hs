module ValidateBST where

data Bin a = Empty | Node a (Bin a) (Bin a)
           deriving Show

empty :: Bin a
empty = Empty

null :: Bin a -> Bool
null Empty = True
null _     = False

singleton :: a -> Bin a
singleton a = Node a Empty Empty

inOrder :: Ord a => Bin a -> [a]
inOrder Empty = []
inOrder (Node a l r) = (inOrder l) ++ [a]++ (inOrder r)

isBST :: Ord a => Bin a -> Bool
isBST = isSorted . inOrder
  where isSorted []  = True
        isSorted [x] = True
        isSorted (x1:x2:xs) | x1 < x2 = isSorted (x2:xs)
                            | otherwise = False

testTree :: Bin Int
testTree = Node 5
           (Node 3 (singleton 2) (singleton 4))
           (Node 7 (singleton 6) (Node 8 Empty (singleton 9)))

main :: IO ()
main = do
  print testTree
  print $ isBST testTree

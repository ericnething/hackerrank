module PartitionList where

import Data.List

partitionList :: Ord a => a -> [a] -> [a]
partitionList a xs = filter (< a) xs ++ filter (>= a) xs

main :: IO ()
main = do
  let a1 = "A girl is Arya Stark"
      val = 'i'
  putStrLn $ show a1 ++ " partitioned around " ++ show val ++
    " becomes " ++ show (partitionList val a1)

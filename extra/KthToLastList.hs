module KthToLastList where

import Data.List

kthToLast :: Int -> [a] -> a
kthToLast k xs = (reverse xs) !! k

main :: IO ()
main = do
  let a = ['a'..'z']
      k = 6
  putStrLn $ "The element " ++ show k ++ " from the end of " ++
    show a ++ " is " ++ show (kthToLast k a)

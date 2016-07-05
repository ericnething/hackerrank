module DeleteNode where

import Data.List

deleteNode :: [a] -> [a]
deleteNode (x:xs) = xs

-- Delete node 'c' from "abcdefg"
test :: [a] -> [a]
test (x0:x1:x2:xs) = x0 : x1 : deleteNode (x2:xs)
test [] = []

main :: IO ()
main = do
  let a1 = "abcdefg"
  putStrLn $ "Delete 'c' from " ++ show a1 ++ ": " ++ show (test a1)
  

module ReplaceSpaces where

import Data.List

replaceSpaces :: String -> String
replaceSpaces = foldr g []
  where g x acc | x == ' ' = "%20" ++ acc
                | otherwise = x:acc

main :: IO ()
main = do
  let a = "A girl is Arya Stark."
      b = "You know nothing Jon Snow."
  putStrLn a
  putStrLn $ replaceSpaces a
  putStrLn b
  putStrLn $ replaceSpaces b

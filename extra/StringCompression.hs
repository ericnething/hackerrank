module StringCompression where

import Data.List

groupSeq :: String -> [String]
groupSeq = foldr g []
  where g x ((a:as):acc) | x == a = (x:a:as):acc
                         | otherwise = [x] : (a:as):acc
        g x [] = [[x]]

compress :: String -> String
compress s | length (result s) < length s = result s
           | otherwise = s
  where result = concatMap (\x -> take 1 x ++ (show $ length x)) . groupSeq

main :: IO ()
main = do
  let a = "aabcccccaaa"
      b = ['a'..'z']
  putStrLn a
  putStrLn $ compress a
  putStrLn b
  putStrLn $ compress b

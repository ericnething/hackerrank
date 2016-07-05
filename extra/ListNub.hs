module ListNub where

import           Data.List

import           Data.Set (Set)
import qualified Data.Set as S

-- 1. Remove duplicates from an unsorted linked list.

-- 2. How would you solve this without using a temporary buffer?

-- Solution 1: Use a Set to store each element as you fold over the
-- list. When you encounter an element that already exists in the Set,
-- remove it from the list. Requires O(n) time, O(m) space, where m is
-- the number of unique elements. -}

listNub :: (Eq a, Ord a) => [a] -> [a]
listNub = reverse . snd . foldl' g (S.empty, [])
  where g (set, acc) x | S.member x set = (set, acc)
                       | otherwise = (S.insert x set, x : acc)

main2 :: IO ()
main2 = do
  let a1 = "Hello world!"
      a2 = "abbcdaefg"
      a3 = "Hello"

  putStrLn a1
  putStrLn $ listNub a1

  putStrLn a2
  putStrLn $ listNub a2

  putStrLn a3
  putStrLn $ listNub a3

-- Solution 2: Iterate through the list and check if each element is
-- used more than once, if it is, remove the duplicates. Requires
-- O(n^2) time, O(1) space.

addUniqueToList :: Eq a => a -> [a] -> [a]
addUniqueToList a xs | a `notElem` xs = a : xs
               | otherwise = xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = reverse . foldl' (flip addUniqueToList) []

showResults :: (Show a, Eq a) => [a] -> String
showResults xs = "Original: " ++ show xs ++ "\n" ++
                 "After: " ++ show (removeDuplicates xs) ++ "\n"

main :: IO ()
main = do
  let a1 = "hello world!"
      a2 = "The quick brown fox jumps over the lazy dog."
      a3 = "abcadefg"
      
  putStrLn $ showResults a1
  putStrLn $ showResults a2
  putStrLn $ showResults a3
  

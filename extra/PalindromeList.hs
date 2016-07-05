module PalindromeList where

import Data.List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (reverse xs) == xs

main :: IO ()
main = do
  let a1 = "airanaria"
      a2 = "Airanaria"
      
  putStrLn $ "Is " ++ show a1 ++ " a palindrome?"
  putStrLn $ show $ isPalindrome a1

  putStrLn $ "Is " ++ show a2 ++ " a palindrome?"
  putStrLn $ show $ isPalindrome a2

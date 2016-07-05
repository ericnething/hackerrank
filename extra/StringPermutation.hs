module StringPermutation where

import Data.List

isPermutationOf :: String -> String -> Bool
isPermutationOf s1 s2 = sort s1 == sort s2

check :: String -> String -> String
check s1 s2 = show s1 ++ result bool ++ "a permutation of " ++ show s2
  where bool = isPermutationOf s1 s2
        result True = " is "
        result _    = " is not "

main :: IO ()
main = do
  let a1 = "Winter is coming."
      a2 = "coming Winter is."
      b1 = "You know nothing Jon Snow."
      b2 = "A girl is Arya Stark."
  putStrLn $ check a1 a2
  putStrLn $ check b1 b2

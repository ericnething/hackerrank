module UniqueString where

import Data.List

checkAdjacentUnique :: String -> Bool
checkAdjacentUnique (x1:x2:xs)
  | x1 == x2 = False
  | otherwise = checkAdjacentUnique (x2:xs)
checkAdjacentUnique _ = True

check :: String -> String
check s = show s ++ result bool ++ "all unique characters."
  where bool = checkAdjacentUnique . sort $ s
        result True = " has "
        result _ = " does not have "

main :: IO ()
main = do
  let testString1 = "Hello World!"
      testString2 = "Hexyz World!"
  putStrLn $ check testString1
  putStrLn $ check testString2

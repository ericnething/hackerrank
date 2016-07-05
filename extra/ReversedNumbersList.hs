module ReversedNumbersList where

import Data.List

rListToNumber :: [Int] -> Int
rListToNumber = foldr g 0
  where g x acc = (acc * 10) + x

numberToRList :: Int -> [Int]
numberToRList = unfoldr g
  where g x | x /= 0 = Just (x `mod` 10, x `div` 10)
            | otherwise = Nothing

main :: IO ()
main = do
  let as = [7,1,1]
      bs = [4,2,3]
      a = rListToNumber as
      b = rListToNumber bs
      
  -- 117 + 324 = 441
  putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show (a + b)
  -- [1,4,4]
  putStrLn $ "In reversed list notation: " ++ show (numberToRList (a + b))

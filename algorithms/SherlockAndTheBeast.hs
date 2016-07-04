module SherlockAndTheBeast where

import Control.Applicative

getValue :: Int -> Maybe String
getValue n = go (n `mod` 3)
  where

    go openSlots
      | openSlots `mod` 5 == 0 = Just $
                                 solution ((n - openSlots) `div` 3) fives ++
                                 solution (openSlots `div` 5)       threes
      | openSlots >= n = Nothing
      | otherwise = go (openSlots + 3)
    
    fives  = "555"
    threes = "33333"
    solution n v = concat $ replicate n v
                                      
run :: IO ()
run = do
  n <- (read :: String -> Int) <$> getLine
  putStrLn $ maybe ("-1") id (getValue n)

main :: IO ()
main = do
  n <- readLn :: IO Int
  mapM_ (\_ -> run) [1..n]

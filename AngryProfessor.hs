module AngryProfessor where

import Control.Applicative
import System.IO

isCancelled :: Int -> [Int] -> Bool
isCancelled k v = (length $ filter (<= 0) v) < k

run :: IO ()
run = do
  (n:k:_) <- fmap (read :: String -> Int) . words <$> getLine
  array <- fmap (read :: String -> Int) . words <$> getLine
  let result = case isCancelled k array of
        True -> "YES"
        False -> "NO"
  putStrLn result

main :: IO ()
main = do
  n <- readLn :: IO Int
  mapM_ (\_ -> run) [1..n]
